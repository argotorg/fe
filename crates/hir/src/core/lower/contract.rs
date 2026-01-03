use parser::ast::{self, AttrListOwner as _, ItemModifierOwner as _, prelude::AstNode as _};

use crate::{
    hir_def::{
        AttrListId, Body, BodyKind, Contract, ContractInit, ContractRecv, ContractRecvArm,
        ContractRecvArmListId, ContractRecvListId, Expr, FieldDef, FieldDefListId, FuncParamListId,
        IdentId, ItemModifier, Pat, TrackedItemVariant, TypeId, Visibility,
    },
    lower::{FileLowerCtxt, body::BodyCtxt, item::lower_uses_clause_opt},
    span::HirOrigin,
};

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

        let init = ast
            .init_block()
            .map(|init_ast| lower_contract_init(ctxt, init_ast));

        // Recv blocks (message handlers)
        let recvs = {
            let mut data = Vec::new();
            for (recv_idx, r) in ast.recvs().enumerate() {
                let msg_path = r.path().map(|p| crate::hir_def::PathId::lower_ast(ctxt, p));
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
            init,
            recvs,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(contract)
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
                .map(|field| lower_contract_field_def(ctxt, field))
                .collect::<Vec<_>>();
            FieldDefListId::new(ctxt.db(), fields)
        }
        None => FieldDefListId::new(ctxt.db(), Vec::new()),
    }
}

fn lower_contract_field_def<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: ast::RecordFieldDef,
) -> FieldDef<'db> {
    let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
    let name = IdentId::lower_token_partial(ctxt, ast.name());
    let type_ref = TypeId::lower_ast_partial(ctxt, ast.ty());
    let vis = if ast.pub_kw().is_some() {
        Visibility::Public
    } else {
        Visibility::Private
    };

    FieldDef::new(attributes, name, type_ref, vis)
}

fn lower_contract_init<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    init_ast: ast::ContractInit,
) -> ContractInit<'db> {
    let db = ctxt.db();

    let id = ctxt.joined_id(TrackedItemVariant::ContractInit);
    let params = init_ast
        .params()
        .map(|p| FuncParamListId::lower_ast(ctxt, p))
        .unwrap_or_else(|| FuncParamListId::new(db, vec![]));
    let effects = lower_uses_clause_opt(ctxt, init_ast.uses_clause());
    let body_ast = init_ast
        .body()
        .map(|b| ast::Expr::cast(b.syntax().clone()).unwrap());
    let body = Body::lower_ast_with_variant(
        ctxt,
        body_ast.clone(),
        TrackedItemVariant::ContractInit,
        BodyKind::FuncBody,
    );
    let origin = HirOrigin::raw(&init_ast);

    ContractInit::new(db, id, params, effects, body, ctxt.top_mod(), origin)
}
