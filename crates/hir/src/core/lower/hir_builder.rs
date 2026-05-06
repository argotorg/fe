use common::ingot::IngotKind;
use parser::ast;
use parser::ast::prelude::AstNode;

use crate::{
    HirDb,
    hir_def::{
        ArithBinOp, AssocConstDef, AssocTyDef, AttrListId, BinOp, Body, BodyKind, Cond,
        EffectParamListId, Expr, ExprId, FieldDefListId, FieldIndex, Func, FuncModifiers,
        FuncParam, FuncParamListId, FuncParamMode, FuncParamName, GenericArg, GenericArgListId,
        GenericParam, GenericParamListId, IdentId, ImplTrait, ItemKind, Mod, Partial, Pat, PatId,
        PathId, PathKind, Stmt, StmtId, Struct, TopLevelMod, TrackedItemId, TrackedItemVariant,
        TraitRefId, TypeBound, TypeGenericArg, TypeGenericParam, TypeId, TypeKind, TypeMode, UnOp,
        Visibility, WhereClauseId, expr::CallArg,
    },
    span::{DesugaredOrigin, HirOrigin},
};

use super::{FileLowerCtxt, body::BodyCtxt};

#[derive(Clone, Copy, Debug)]
pub(super) struct LibRoots<'db> {
    pub(super) core: IdentId<'db>,
    pub(super) std: IdentId<'db>,
}

impl<'db> LibRoots<'db> {
    pub(super) fn for_ctxt(ctxt: &FileLowerCtxt<'db>) -> Self {
        let db = ctxt.db();
        let ingot = ctxt.top_mod().ingot(db);

        let core = if ingot.kind(db) == IngotKind::Core {
            IdentId::make_ingot(db)
        } else {
            IdentId::make_core(db)
        };
        let std = if ingot.kind(db) == IngotKind::Std {
            IdentId::make_ingot(db)
        } else {
            IdentId::new(db, "std".to_string())
        };

        Self { core, std }
    }
}

pub(super) struct HirBuilder<'ctxt, 'db, O>
where
    O: Clone + Into<DesugaredOrigin>,
{
    ctxt: &'ctxt mut FileLowerCtxt<'db>,
    roots: LibRoots<'db>,
    desugared: O,
}

impl<'ctxt, 'db, O> HirBuilder<'ctxt, 'db, O>
where
    O: Clone + Into<DesugaredOrigin>,
{
    pub(super) fn new(ctxt: &'ctxt mut FileLowerCtxt<'db>, desugared: O) -> Self {
        let roots = LibRoots::for_ctxt(ctxt);
        Self {
            ctxt,
            roots,
            desugared,
        }
    }

    pub(super) fn db(&self) -> &'db dyn HirDb {
        self.ctxt.db()
    }

    pub(super) fn ctxt(&mut self) -> &mut FileLowerCtxt<'db> {
        self.ctxt
    }

    pub(super) fn top_mod(&self) -> TopLevelMod<'db> {
        self.ctxt.top_mod()
    }

    pub(super) fn ident(&self, name: &str) -> IdentId<'db> {
        IdentId::new(self.db(), name.to_string())
    }

    pub(super) fn roots(&self) -> LibRoots<'db> {
        self.roots
    }

    pub(super) fn origin<T>(&self) -> HirOrigin<T>
    where
        T: AstNode<Language = parser::FeLang>,
    {
        HirOrigin::desugared(self.desugared.clone())
    }

    pub(super) fn with_desugared<O2>(&mut self, desugared: O2) -> HirBuilder<'_, 'db, O2>
    where
        O2: Clone + Into<DesugaredOrigin>,
    {
        HirBuilder {
            ctxt: self.ctxt,
            roots: self.roots,
            desugared,
        }
    }

    pub(super) fn empty_attrs(&self) -> AttrListId<'db> {
        AttrListId::new(self.db(), vec![])
    }

    pub(super) fn empty_generic_params(&self) -> GenericParamListId<'db> {
        GenericParamListId::new(self.db(), vec![])
    }

    pub(super) fn empty_where_clause(&self) -> WhereClauseId<'db> {
        WhereClauseId::new(self.db(), vec![])
    }

    pub(super) fn empty_effect_params(&self) -> EffectParamListId<'db> {
        EffectParamListId::new(self.db(), vec![])
    }

    pub(super) fn self_ty(&self) -> TypeId<'db> {
        TypeId::fallback_self_ty(self.db())
    }

    pub(super) fn path_from_root(&self, root: IdentId<'db>, segments: &[&str]) -> PathId<'db> {
        let db = self.db();
        let mut path = PathId::from_ident(db, root);
        for seg in segments {
            path = path.push_str(db, seg);
        }
        path
    }

    pub(super) fn ty_path(&self, path: PathId<'db>) -> TypeId<'db> {
        TypeId::new(self.db(), TypeKind::Path(Partial::Present(path)))
    }

    pub(super) fn ty_ident(&self, ident: IdentId<'db>) -> TypeId<'db> {
        self.ty_path(PathId::from_ident(self.db(), ident))
    }

    pub(super) fn sol_ty(&self) -> TypeId<'db> {
        let path = self.path_from_root(self.roots.std, &["abi", "Sol"]);
        self.ty_path(path)
    }

    pub(super) fn sol_args(&self) -> GenericArgListId<'db> {
        GenericArgListId::given1_type(self.db(), self.sol_ty())
    }

    pub(super) fn core_abi_trait_ref_sol(&self, name: &str) -> TraitRefId<'db> {
        let db = self.db();
        let path = PathId::from_ident(db, self.roots.core)
            .push_str(db, "abi")
            .push_str_args(db, name, self.sol_args());
        TraitRefId::new(db, Partial::Present(path))
    }

    pub(super) fn type_param_with_trait_bound(
        &self,
        name: &str,
        bound: TraitRefId<'db>,
    ) -> (GenericParamListId<'db>, TypeId<'db>) {
        let db = self.db();
        let ident = self.ident(name);
        let params = GenericParamListId::new(
            db,
            vec![GenericParam::Type(TypeGenericParam {
                name: Partial::Present(ident),
                bounds: vec![TypeBound::Trait(bound)],
                default_ty: None,
            })],
        );
        (params, self.ty_ident(ident))
    }

    pub(super) fn param_own_self(&self) -> FuncParam<'db> {
        let db = self.db();
        FuncParam {
            mode: FuncParamMode::Own,
            is_mut: false,
            has_ref_prefix: false,
            has_own_prefix: false,
            is_label_suppressed: false,
            name: Partial::Present(FuncParamName::Ident(IdentId::make_self(db))),
            ty: Partial::Present(self.self_ty()),
            self_ty_fallback: true,
        }
    }

    pub(super) fn param_mut_underscore_named(
        &self,
        name: IdentId<'db>,
        ty: TypeId<'db>,
    ) -> FuncParam<'db> {
        FuncParam {
            mode: FuncParamMode::View,
            is_mut: false,
            has_ref_prefix: false,
            has_own_prefix: false,
            is_label_suppressed: true,
            name: Partial::Present(FuncParamName::Ident(name)),
            ty: Partial::Present(TypeId::new(
                self.db(),
                TypeKind::Mode(TypeMode::Mut, Partial::Present(ty)),
            )),
            self_ty_fallback: false,
        }
    }

    pub(super) fn param_underscore_named(
        &self,
        name: IdentId<'db>,
        ty: TypeId<'db>,
    ) -> FuncParam<'db> {
        FuncParam {
            mode: FuncParamMode::View,
            is_mut: false,
            has_ref_prefix: false,
            has_own_prefix: false,
            is_label_suppressed: true,
            name: Partial::Present(FuncParamName::Ident(name)),
            ty: Partial::Present(ty),
            self_ty_fallback: false,
        }
    }

    pub(super) fn params(
        &self,
        params: impl IntoIterator<Item = FuncParam<'db>>,
    ) -> FuncParamListId<'db> {
        FuncParamListId::new(self.db(), params.into_iter().collect::<Vec<_>>())
    }

    pub(super) fn assoc_ty(&self, name: &str, type_ref: Partial<TypeId<'db>>) -> AssocTyDef<'db> {
        AssocTyDef {
            attributes: self.empty_attrs(),
            name: Partial::Present(self.ident(name)),
            type_ref,
        }
    }

    pub(super) fn with_mod_scope<R>(
        &mut self,
        id: TrackedItemVariant<'db>,
        build: impl FnOnce(&mut Self, TrackedItemId<'db>) -> R,
    ) -> R {
        let id = self.ctxt.joined_id(id);
        self.ctxt.enter_item_scope(id, true);
        build(self, id)
    }

    pub(super) fn with_item_scope<I>(
        &mut self,
        id: TrackedItemVariant<'db>,
        build: impl FnOnce(&mut Self, TrackedItemId<'db>) -> I,
    ) -> I
    where
        I: Into<ItemKind<'db>> + Copy,
    {
        let id = self.ctxt.joined_id(id);
        self.ctxt.enter_item_scope(id, false);
        let item = build(self, id);
        self.ctxt.leave_item_scope(item);
        item
    }

    pub(super) fn finish_mod(
        &mut self,
        id: TrackedItemId<'db>,
        name: Partial<IdentId<'db>>,
        attributes: AttrListId<'db>,
        vis: Visibility,
        origin: HirOrigin<ast::Mod>,
    ) -> Mod<'db> {
        let mod_ = Mod::new(self.db(), id, name, attributes, vis, self.top_mod(), origin);
        self.ctxt.leave_item_scope(mod_)
    }

    pub(super) fn desugared_mod(
        &mut self,
        name: Partial<IdentId<'db>>,
        attributes: AttrListId<'db>,
        vis: Visibility,
        build: impl FnOnce(&mut Self),
    ) -> Mod<'db> {
        self.with_mod_scope(TrackedItemVariant::Mod(name), |this, id| {
            this.ctxt.insert_synthetic_prelude_use();
            this.ctxt.insert_synthetic_super_use();
            build(this);
            this.finish_mod(id, name, attributes, vis, this.origin())
        })
    }

    pub(super) fn struct_item(
        &mut self,
        name: Partial<IdentId<'db>>,
        attributes: AttrListId<'db>,
        vis: Visibility,
        generic_params: GenericParamListId<'db>,
        where_clause: WhereClauseId<'db>,
        fields: FieldDefListId<'db>,
    ) -> Struct<'db> {
        self.with_item_scope(TrackedItemVariant::Struct(name), |this, id| {
            Struct::new(
                this.db(),
                id,
                name,
                attributes,
                vis,
                generic_params,
                where_clause,
                fields,
                this.top_mod(),
                this.origin(),
            )
        })
    }

    pub(super) fn struct_simple(
        &mut self,
        name: Partial<IdentId<'db>>,
        attributes: AttrListId<'db>,
        vis: Visibility,
        fields: FieldDefListId<'db>,
    ) -> Struct<'db> {
        self.struct_item(
            name,
            attributes,
            vis,
            self.empty_generic_params(),
            self.empty_where_clause(),
            fields,
        )
    }

    pub(super) fn pub_struct(
        &mut self,
        name: Partial<IdentId<'db>>,
        attributes: AttrListId<'db>,
        fields: FieldDefListId<'db>,
    ) -> Struct<'db> {
        self.struct_simple(name, attributes, Visibility::Public, fields)
    }

    pub(super) fn new_impl_trait(
        &mut self,
        id: TrackedItemId<'db>,
        trait_ref: Partial<TraitRefId<'db>>,
        ty: Partial<TypeId<'db>>,
        types: Vec<AssocTyDef<'db>>,
        consts: Vec<AssocConstDef<'db>>,
        origin: HirOrigin<ast::ImplTrait>,
    ) -> ImplTrait<'db> {
        let attrs = self.empty_attrs();
        let generic_params = self.empty_generic_params();
        let where_clause = self.empty_where_clause();

        ImplTrait::new(
            self.db(),
            id,
            trait_ref,
            ty,
            attrs,
            generic_params,
            where_clause,
            types,
            consts,
            self.top_mod(),
            origin,
        )
    }

    pub(super) fn impl_trait(
        &mut self,
        trait_ref: TraitRefId<'db>,
        ty: TypeId<'db>,
        build_children: impl FnOnce(&mut Self),
    ) -> ImplTrait<'db> {
        self.impl_trait_with_children(trait_ref, ty, vec![], vec![], build_children)
    }

    pub(super) fn impl_trait_assocs_build(
        &mut self,
        trait_ref: TraitRefId<'db>,
        ty: TypeId<'db>,
        build_assocs: impl FnOnce(&mut Self) -> (Vec<AssocTyDef<'db>>, Vec<AssocConstDef<'db>>),
    ) -> ImplTrait<'db> {
        let trait_ref = Partial::Present(trait_ref);
        let ty = Partial::Present(ty);

        let idx = self.ctxt.next_impl_trait_idx();
        self.with_item_scope(TrackedItemVariant::ImplTrait(idx), |this, id| {
            let (types, consts) = build_assocs(this);
            this.new_impl_trait(id, trait_ref, ty, types, consts, this.origin())
        })
    }

    pub(super) fn impl_trait_with_children(
        &mut self,
        trait_ref: TraitRefId<'db>,
        ty: TypeId<'db>,
        types: Vec<AssocTyDef<'db>>,
        consts: Vec<AssocConstDef<'db>>,
        build_children: impl FnOnce(&mut Self),
    ) -> ImplTrait<'db> {
        let trait_ref = Partial::Present(trait_ref);
        let ty = Partial::Present(ty);

        let idx = self.ctxt.next_impl_trait_idx();
        self.with_item_scope(TrackedItemVariant::ImplTrait(idx), |this, id| {
            let impl_trait = this.new_impl_trait(id, trait_ref, ty, types, consts, this.origin());
            build_children(this);
            impl_trait
        })
    }

    pub(super) fn func_with_body(
        &mut self,
        name: IdentId<'db>,
        generic_params: GenericParamListId<'db>,
        params: FuncParamListId<'db>,
        ret_ty: Option<TypeId<'db>>,
        modifiers: FuncModifiers,
        build_body: impl FnOnce(&mut BodyBuilder<'_, 'db, O>),
    ) -> Func<'db> {
        let attrs = self.empty_attrs();
        let where_clause = self.empty_where_clause();
        let effects = self.empty_effect_params();
        self.with_item_scope(
            TrackedItemVariant::Func(Partial::Present(name)),
            |this, id| {
                let mut body_builder = BodyBuilder::new(
                    this.ctxt,
                    this.roots,
                    this.desugared.clone(),
                    TrackedItemVariant::FuncBody,
                );
                build_body(&mut body_builder);
                let body = body_builder.finish();

                Func::new(
                    this.db(),
                    id,
                    Partial::Present(name),
                    attrs,
                    generic_params,
                    where_clause,
                    Partial::Present(params),
                    effects,
                    ret_ty,
                    modifiers,
                    Some(body),
                    this.top_mod(),
                    this.origin(),
                )
            },
        )
    }

    pub(super) fn func_generic(
        &mut self,
        name: &str,
        generic_params: GenericParamListId<'db>,
        params: FuncParamListId<'db>,
        ret_ty: Option<TypeId<'db>>,
        modifiers: FuncModifiers,
        build_body: impl FnOnce(&mut BodyBuilder<'_, 'db, O>),
    ) -> Func<'db> {
        self.func_with_body(
            self.ident(name),
            generic_params,
            params,
            ret_ty,
            modifiers,
            build_body,
        )
    }
}

pub(super) struct BodyBuilder<'ctxt, 'db, O>
where
    O: Clone + Into<DesugaredOrigin>,
{
    body: BodyCtxt<'ctxt, 'db>,
    roots: LibRoots<'db>,
    desugared: O,
    stmts: Vec<StmtId>,
}

impl<'ctxt, 'db, O> BodyBuilder<'ctxt, 'db, O>
where
    O: Clone + Into<DesugaredOrigin>,
{
    fn new(
        ctxt: &'ctxt mut FileLowerCtxt<'db>,
        roots: LibRoots<'db>,
        desugared: O,
        id: TrackedItemVariant<'db>,
    ) -> Self {
        let id = ctxt.joined_id(id);
        Self {
            body: BodyCtxt::new(ctxt, id),
            roots,
            desugared,
            stmts: Vec::new(),
        }
    }

    pub(super) fn db(&self) -> &'db dyn HirDb {
        self.body.f_ctxt.db()
    }

    fn sol_ty(&self) -> TypeId<'db> {
        let path = PathId::from_ident(self.db(), self.roots.std)
            .push_str(self.db(), "abi")
            .push_str(self.db(), "Sol");
        TypeId::new(self.db(), TypeKind::Path(Partial::Present(path)))
    }

    fn abi_size_trait_ref(&self) -> TraitRefId<'db> {
        let path = PathId::from_ident(self.db(), self.roots.core)
            .push_str(self.db(), "abi")
            .push_str(self.db(), "AbiSize");
        TraitRefId::new(self.db(), Partial::Present(path))
    }

    fn expr_origin(&self) -> HirOrigin<ast::Expr> {
        HirOrigin::desugared(self.desugared.clone())
    }

    fn stmt_origin(&self) -> HirOrigin<ast::Stmt> {
        HirOrigin::desugared(self.desugared.clone())
    }

    fn pat_origin(&self) -> HirOrigin<ast::Pat> {
        HirOrigin::desugared(self.desugared.clone())
    }

    pub(super) fn push_expr(&mut self, expr: Expr<'db>) -> ExprId {
        self.body.push_expr(expr, self.expr_origin())
    }

    pub(super) fn push_pat(&mut self, pat: Pat<'db>) -> PatId {
        self.body.push_pat(pat, self.pat_origin())
    }

    pub(super) fn push_stmt_raw(&mut self, stmt: Stmt<'db>) -> StmtId {
        self.body.push_stmt(stmt, self.stmt_origin())
    }

    pub(super) fn emit_stmt(&mut self, stmt: Stmt<'db>) -> StmtId {
        let stmt_id = self.push_stmt_raw(stmt);
        self.stmts.push(stmt_id);
        stmt_id
    }

    pub(super) fn emit_expr_stmt(&mut self, expr: ExprId) -> StmtId {
        self.emit_stmt(Stmt::Expr(expr))
    }

    pub(super) fn emit_return(&mut self, expr: Option<ExprId>) -> StmtId {
        self.emit_stmt(Stmt::Return(expr))
    }

    pub(super) fn ident_expr(&mut self, ident: IdentId<'db>) -> ExprId {
        self.push_expr(Expr::Path(Partial::Present(PathId::from_ident(
            self.db(),
            ident,
        ))))
    }

    pub(super) fn path_expr(&mut self, path: PathId<'db>) -> ExprId {
        self.push_expr(Expr::Path(Partial::Present(path)))
    }

    pub(super) fn abi_size_assoc_expr(&mut self, ty: TypeId<'db>, assoc_name: &str) -> ExprId {
        let qualified = PathId::new(
            self.db(),
            PathKind::QualifiedType {
                type_: ty,
                trait_: self.abi_size_trait_ref(),
            },
            None,
        );
        self.path_expr(qualified.push_str(self.db(), assoc_name))
    }

    pub(super) fn call_expr(&mut self, callee: ExprId, args: Vec<ExprId>) -> ExprId {
        self.call_expr_with_args(
            callee,
            args.into_iter()
                .map(|expr| CallArg { label: None, expr })
                .collect(),
        )
    }

    pub(super) fn call_expr_with_args(
        &mut self,
        callee: ExprId,
        args: Vec<CallArg<'db>>,
    ) -> ExprId {
        self.push_expr(Expr::Call(callee, args))
    }

    pub(super) fn method_call_expr(
        &mut self,
        receiver: ExprId,
        name: IdentId<'db>,
        args: Vec<ExprId>,
    ) -> ExprId {
        self.method_call_expr_with_args(
            receiver,
            name,
            args.into_iter()
                .map(|expr| CallArg { label: None, expr })
                .collect(),
        )
    }

    pub(super) fn method_call_expr_with_args(
        &mut self,
        receiver: ExprId,
        name: IdentId<'db>,
        args: Vec<CallArg<'db>>,
    ) -> ExprId {
        self.push_expr(Expr::MethodCall(
            receiver,
            Partial::Present(name),
            GenericArgListId::none(self.db()),
            args,
        ))
    }

    pub(super) fn if_expr(
        &mut self,
        cond_expr: ExprId,
        then_expr: ExprId,
        else_expr: ExprId,
    ) -> ExprId {
        let cond = self.body.push_cond(Cond::Expr(cond_expr));
        // Fe `if` expressions require block bodies. When constructing these
        // blocks synthetically, we must also register them with the
        // `ScopeGraphBuilder` so name resolution has a scope entry.
        let wrap = |this: &mut Self, expr: ExprId| -> ExprId {
            this.body.f_ctxt.enter_block_scope();
            let stmt = this.push_stmt_raw(Stmt::Expr(expr));
            let block = this.push_expr(Expr::Block(vec![stmt]));
            this.body.f_ctxt.leave_block_scope(block);
            block
        };
        let then_expr = wrap(self, then_expr);
        let else_expr = wrap(self, else_expr);
        self.push_expr(Expr::If(cond, then_expr, Some(else_expr)))
    }

    pub(super) fn abi_field_head_size_expr(&mut self, ty: TypeId<'db>) -> ExprId {
        let db = self.db();
        let args = GenericArgListId::given(
            db,
            vec![GenericArg::Type(TypeGenericArg {
                ty: Partial::Present(ty),
            })],
        );
        let path = PathId::from_ident(db, self.roots.core)
            .push_str(db, "abi")
            .push_str_args(db, "abi_field_head_size", args);
        let callee = self.path_expr(path);
        self.call_expr(callee, vec![])
    }

    pub(super) fn abi_field_validate_end_expr(
        &mut self,
        field_ty: TypeId<'db>,
        input_ty: TypeId<'db>,
        input: ExprId,
        base: ExprId,
        pos: ExprId,
    ) -> ExprId {
        let db = self.db();
        let args = GenericArgListId::given(
            db,
            vec![
                GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(self.sol_ty()),
                }),
                GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(field_ty),
                }),
                GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(input_ty),
                }),
            ],
        );
        let path = PathId::from_ident(db, self.roots.core)
            .push_str(db, "abi")
            .push_str_args(db, "abi_field_validate_end", args);
        let callee = self.path_expr(path);
        self.call_expr_with_args(
            callee,
            vec![
                CallArg {
                    label: None,
                    expr: input,
                },
                CallArg {
                    label: Some(IdentId::new(db, "base".to_string())),
                    expr: base,
                },
                CallArg {
                    label: Some(IdentId::new(db, "pos".to_string())),
                    expr: pos,
                },
            ],
        )
    }

    pub(super) fn encode_fields(
        &mut self,
        fields: &[(IdentId<'db>, TypeId<'db>)],
        encoder_ident: IdentId<'db>,
        encoder_ty: TypeId<'db>,
    ) {
        if fields.is_empty() {
            return;
        }

        let db = self.db();
        let self_expr = self.path_expr(PathId::from_ident(db, IdentId::make_self(db)));
        let tail_ident = IdentId::new(db, "__tail".to_string());
        let base_ident = IdentId::new(db, "base".to_string());
        let dynamic_payload_size_path = PathId::from_ident(db, self.roots.core)
            .push_str(db, "abi")
            .push_str(db, "dynamic_payload_size");
        let head_size = self.abi_size_assoc_expr(TypeId::fallback_self_ty(db), "HEAD_SIZE");
        let encoder_expr = self.ident_expr(encoder_ident);
        let base_expr = self.method_call_expr(encoder_expr, base_ident, vec![]);
        let tail_init = self.push_expr(Expr::Bin(
            base_expr,
            head_size,
            BinOp::Arith(ArithBinOp::Add),
        ));
        let tail_pat = self.push_pat(Pat::Path(
            Partial::Present(PathId::from_ident(db, tail_ident)),
            true,
        ));
        self.emit_stmt(Stmt::Let(tail_pat, None, Some(tail_init)));
        for (index, (field, field_ty)) in fields.iter().copied().enumerate() {
            let field_ident = IdentId::new(db, format!("__field_{index}"));
            let field_pat = self.push_pat(Pat::Path(
                Partial::Present(PathId::from_ident(db, field_ident)),
                false,
            ));
            let receiver = self.push_expr(Expr::Field(
                self_expr,
                Partial::Present(FieldIndex::Ident(field)),
            ));
            self.emit_stmt(Stmt::Let(field_pat, None, Some(receiver)));
            let field_expr = self.ident_expr(field_ident);
            let dynamic_payload_size = self.path_expr(dynamic_payload_size_path);
            let field_size = self.call_expr(dynamic_payload_size, vec![field_expr]);
            let encode_field_args = GenericArgListId::given(
                db,
                vec![
                    GenericArg::Type(TypeGenericArg {
                        ty: Partial::Present(self.sol_ty()),
                    }),
                    GenericArg::Type(TypeGenericArg {
                        ty: Partial::Present(field_ty),
                    }),
                    GenericArg::Type(TypeGenericArg {
                        ty: Partial::Present(encoder_ty),
                    }),
                ],
            );
            let encode_field_path = PathId::from_ident(db, self.roots.core)
                .push_str(db, "abi")
                .push_str_args(db, "encode_field", encode_field_args);
            let encode_field_callee = self.path_expr(encode_field_path);
            let field_value = self.ident_expr(field_ident);
            let encoder_arg = self.ident_expr(encoder_ident);
            let tail_value = self.ident_expr(tail_ident);
            let tail_arg = self.push_expr(Expr::Un(tail_value, UnOp::Mut));
            let call = self.call_expr(
                encode_field_callee,
                vec![field_value, encoder_arg, tail_arg],
            );
            self.emit_expr_stmt(call);
            let tail_value = self.ident_expr(tail_ident);
            let tail_next = self.push_expr(Expr::Bin(
                tail_value,
                field_size,
                BinOp::Arith(ArithBinOp::Add),
            ));
            let tail_place = self.ident_expr(tail_ident);
            let assign = self.push_expr(Expr::Assign(tail_place, tail_next));
            self.emit_expr_stmt(assign);
        }
    }

    pub(super) fn decode_into(
        &mut self,
        target_ident: IdentId<'db>,
        ty: TypeId<'db>,
        decoder_ty: TypeId<'db>,
    ) {
        let db = self.db();
        let decode_args = GenericArgListId::given(
            db,
            vec![
                GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(self.sol_ty()),
                }),
                GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(ty),
                }),
                GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(decoder_ty),
                }),
            ],
        );
        let decode_path = PathId::from_ident(db, self.roots.core)
            .push_str(db, "abi")
            .push_str_args(db, "decode_field", decode_args);
        let decode_callee = self.path_expr(decode_path);
        let d_expr = self.path_expr(PathId::from_str(db, "d"));
        let decode_call = self.call_expr(decode_callee, vec![d_expr]);

        let bind_pat = self.push_pat(Pat::Path(
            Partial::Present(PathId::from_ident(db, target_ident)),
            false,
        ));
        self.emit_stmt(Stmt::Let(bind_pat, Some(ty), Some(decode_call)));
    }

    pub(super) fn return_record_self(&mut self, fields: &[IdentId<'db>]) {
        let db = self.db();
        let self_path = Partial::Present(PathId::from_ident(db, IdentId::make_self_ty(db)));
        let fields = fields
            .iter()
            .copied()
            .map(|field_name| crate::hir_def::Field {
                label: Some(field_name),
                expr: self.ident_expr(field_name),
            })
            .collect();
        let record_expr = self.push_expr(Expr::RecordInit(self_path, fields));
        self.emit_return(Some(record_expr));
    }

    fn finish(mut self) -> Body<'db> {
        self.body.f_ctxt.enter_block_scope();
        let stmts = std::mem::take(&mut self.stmts);
        let root_expr = self.push_expr(Expr::Block(stmts));
        self.body.f_ctxt.leave_block_scope(root_expr);
        self.body.build(None, root_expr, BodyKind::FuncBody)
    }
}
