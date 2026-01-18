use common::ingot::IngotKind;
use parser::ast;
use parser::ast::prelude::AstNode;

use crate::{
    HirDb,
    hir_def::{
        AssocConstDef, AssocTyDef, AttrListId, Body, BodyKind, EffectParamListId, Expr, ExprId,
        FieldDefListId, Func, FuncModifiers, FuncParam, FuncParamListId, FuncParamName,
        GenericArgListId, GenericParam, GenericParamListId, IdentId, ImplTrait, IntegerId,
        ItemKind, LitKind, Mod, Partial, Pat, PatId, PathId, RecordPatField, Stmt, StmtId, Struct,
        TopLevelMod, TrackedItemId, TrackedItemVariant, TraitRefId, TypeBound, TypeGenericParam,
        TypeId, TypeKind, Visibility, WhereClauseId, expr::CallArg,
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

    pub(super) fn param_self(&self) -> FuncParam<'db> {
        let db = self.db();
        FuncParam {
            is_mut: false,
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
            is_mut: true,
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
    tmp_counter: usize,
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
            tmp_counter: 0,
        }
    }

    pub(super) fn db(&self) -> &'db dyn HirDb {
        self.body.f_ctxt.db()
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

    pub(super) fn call_expr(&mut self, callee: ExprId, args: Vec<ExprId>) -> ExprId {
        let args = args
            .into_iter()
            .map(|expr| CallArg { label: None, expr })
            .collect();
        self.push_expr(Expr::Call(callee, args))
    }

    pub(super) fn method_call_expr(
        &mut self,
        receiver: ExprId,
        name: IdentId<'db>,
        args: Vec<ExprId>,
    ) -> ExprId {
        let args = args
            .into_iter()
            .map(|expr| CallArg { label: None, expr })
            .collect();
        self.push_expr(Expr::MethodCall(
            receiver,
            Partial::Present(name),
            GenericArgListId::none(self.db()),
            args,
        ))
    }

    pub(super) fn let_self_record(&mut self, fields: &[IdentId<'db>]) {
        if fields.is_empty() {
            return;
        }

        let db = self.db();
        let self_expr = self.path_expr(PathId::from_ident(db, IdentId::make_self(db)));

        let record_fields = fields
            .iter()
            .copied()
            .map(|field_name| {
                let pat = self.push_pat(Pat::Path(
                    Partial::Present(PathId::from_ident(db, field_name)),
                    false,
                ));
                RecordPatField {
                    label: Partial::Present(field_name),
                    pat,
                }
            })
            .collect();

        let bind_pat = self.push_pat(Pat::Record(
            Partial::Present(PathId::from_ident(db, IdentId::make_self_ty(db))),
            record_fields,
        ));
        self.emit_stmt(Stmt::Let(bind_pat, None, Some(self_expr)));
    }

    pub(super) fn encode_fields(&mut self, fields: &[IdentId<'db>], encoder_ident: IdentId<'db>) {
        if fields.is_empty() {
            return;
        }

        let encode_ident = IdentId::new(self.db(), "encode".to_string());
        for field in fields.iter().copied() {
            let receiver = self.ident_expr(field);
            let encoder_expr = self.ident_expr(encoder_ident);
            let call = self.method_call_expr(receiver, encode_ident, vec![encoder_expr]);
            self.emit_expr_stmt(call);
        }
    }

    fn revert_call(&mut self) -> ExprId {
        let db = self.db();
        let revert_path = PathId::from_ident(db, self.roots.core).push_str(db, "revert");
        let revert_callee = self.path_expr(revert_path);
        let zero = Expr::Lit(LitKind::Int(IntegerId::from_usize(db, 0)));
        let arg0 = self.push_expr(zero.clone());
        let arg1 = self.push_expr(zero);
        self.call_expr(revert_callee, vec![arg0, arg1])
    }

    fn fresh_tmp_ident(&mut self) -> IdentId<'db> {
        let ident = IdentId::new(self.db(), format!("__tmp{}", self.tmp_counter));
        self.tmp_counter += 1;
        ident
    }

    pub(super) fn decode_into(&mut self, target_ident: IdentId<'db>, ty: TypeId<'db>) {
        let db = self.db();

        if let TypeKind::Tuple(tup) = ty.data(db) {
            let mut elem_idents = Vec::new();
            for elem_ty in tup.data(db) {
                let Some(elem_ty) = elem_ty.to_opt() else {
                    continue;
                };
                let elem_ident = self.fresh_tmp_ident();
                self.decode_into(elem_ident, elem_ty);
                elem_idents.push(elem_ident);
            }

            let elems = elem_idents
                .into_iter()
                .map(|ident| self.ident_expr(ident))
                .collect();
            let tuple_expr = self.push_expr(Expr::Tuple(elems));

            let bind_pat = self.push_pat(Pat::Path(
                Partial::Present(PathId::from_ident(db, target_ident)),
                false,
            ));
            self.emit_stmt(Stmt::Let(bind_pat, None, Some(tuple_expr)));
            return;
        }

        let Some(ty_path) = (match ty.data(db) {
            TypeKind::Path(p) => p.to_opt(),
            _ => None,
        }) else {
            let revert = self.revert_call();
            let bind_pat = self.push_pat(Pat::Path(
                Partial::Present(PathId::from_ident(db, target_ident)),
                false,
            ));
            self.emit_stmt(Stmt::Let(bind_pat, None, Some(revert)));
            return;
        };

        let decode_path = ty_path.push_str(db, "decode");
        let decode_callee = self.path_expr(decode_path);
        let d_expr = self.path_expr(PathId::from_str(db, "d"));
        let decode_call = self.call_expr(decode_callee, vec![d_expr]);

        let bind_pat = self.push_pat(Pat::Path(
            Partial::Present(PathId::from_ident(db, target_ident)),
            false,
        ));
        self.emit_stmt(Stmt::Let(bind_pat, None, Some(decode_call)));
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
