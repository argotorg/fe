use parser::ast::{self, AttrListOwner as _, prelude::AstNode as _};

use crate::{
    hir_def::{
        AttrListId, BlockKind, Body, BodyKind, Contract, ContractInit, ContractRecv,
        ContractRecvArm, ContractRecvArmListId, ContractRecvListId, Expr, FieldDef, FieldDefListId,
        FuncParamListId, IdentId, Pat, TrackedItemVariant, TypeId,
    },
    lower::{
        FileLowerCtxt,
        attr::{AttrForm, AttrRule, AttrTarget, validate_attr_rules},
        body::BodyCtxt,
        item::lower_uses_clause_opt,
    },
    span::HirOrigin,
};

const SLOT_EXPECTED: &str = "`#[slot(<slot>)]`";
pub(super) const SLOT_TARGETS: &str = "`mut` contract fields";

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
        let body_expr = if let Some(body_ast) = body_ast.clone() {
            Expr::push_to_body_opt(&mut body_ctxt, Some(body_ast))
        } else {
            // `Ping {}` in a recv arm is parsed as a record-pattern arm with no
            // explicit body block. Lower that shorthand as an empty block so
            // later typed/semantic stages never see an absent executable body.
            body_ctxt.f_ctxt.enter_block_scope();
            let body_expr =
                body_ctxt.push_expr(Expr::Block(Vec::new(), BlockKind::Normal), HirOrigin::None);
            body_ctxt.f_ctxt.leave_block_scope(body_expr);
            body_expr
        };
        let body = body_ctxt.build(body_ast.as_ref(), body_expr, BodyKind::FuncBody);
        let ret_ty = ast.ret_ty().map(|ty| TypeId::lower_ast(ctxt, ty));
        let effects = lower_uses_clause_opt(ctxt, ast.uses_clause());
        let attributes =
            super::payable::lower_contract_entry_attrs_opt(ctxt, ast.attr_list(), "recv arm");

        ContractRecvArm {
            pat,
            ret_ty,
            effects,
            body,
            attributes,
        }
    }
}

impl<'db> Contract<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Contract) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Contract(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = super::lower_visibility(&ast);
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
                super::item::report_unsupported_member_attrs(
                    ctxt,
                    r.attr_list(),
                    "recv block",
                    None,
                );
                super::attr::validate_unknown_attrs_in_restricted_context(
                    ctxt,
                    r.attr_list(),
                    super::attr::AttrTarget::new("recv block", None),
                    &["payable"],
                    "no attributes",
                );
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
    super::item::report_unsupported_member_attrs(ctxt, ast.attr_list(), "field", None);
    let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
    let name = IdentId::lower_token_partial(ctxt, ast.name());
    let type_ref = TypeId::lower_ast_partial(ctxt, ast.ty());
    let vis = super::lower_field_visibility(&ast);
    let is_mut = ast.mut_kw().is_some();
    let slot = lower_slot_attr(
        ctxt,
        ast.attr_list(),
        is_mut,
        ast.name().map(|name| name.text().to_string()),
    );

    FieldDef {
        slot,
        ..FieldDef::new(attributes, name, type_ref, vis, false, is_mut)
    }
}

/// Lowers a contract field's `#[slot(e)]` to the body of `e`. An immutable
/// field has no slot: it lives in the contract's code.
fn lower_slot_attr<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    is_mut: bool,
    name: Option<String>,
) -> Option<Body<'db>> {
    let (kind, rule) = if is_mut {
        (
            "contract field",
            AttrRule::supported("slot", AttrForm::SingleExpr, SLOT_EXPECTED),
        )
    } else {
        (
            "immutable contract field",
            AttrRule::unsupported("slot", SLOT_TARGETS),
        )
    };
    validate_attr_rules(ctxt, attrs.clone(), AttrTarget::new(kind, name), &[rule]);
    if !is_mut {
        return None;
    }
    let attr = attrs?.normal_attrs_named("slot").next()?;
    let mut args = attr.args()?.into_iter();
    let (Some(arg), None) = (args.next(), args.next()) else {
        return None;
    };
    match (arg.key(), arg.value()) {
        (None, Some(ast::AttrArgValueKind::Expr(expr))) => {
            Some(Body::lower_ast_nameless(ctxt, expr))
        }
        (Some(path), None) => Some(Body::lower_attr_path(ctxt, path, &arg)),
        _ => None,
    }
}

fn lower_contract_init<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    init_ast: ast::ContractInit,
) -> ContractInit<'db> {
    let db = ctxt.db();

    let attributes =
        super::payable::lower_contract_entry_attrs_opt(ctxt, init_ast.attr_list(), "init block");
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
        body_ast,
        init_ast.params(),
        TrackedItemVariant::ContractInit,
        BodyKind::FuncBody,
    );
    let origin = HirOrigin::raw(&init_ast);

    ContractInit::new(
        db,
        id,
        attributes,
        params,
        effects,
        body,
        ctxt.top_mod(),
        origin,
    )
}
