use std::{collections::BTreeMap, rc::Rc, vec};

use fe_analyzer::{
    builtins::{ContractTypeMethod, GlobalFunction, ValueMethod},
    constants::{EMITTABLE_TRAIT_NAME, EMIT_FN_NAME},
    context::{Adjustment, AdjustmentKind, CallType as AnalyzerCallType, NamedThing},
    namespace::{
        items as analyzer_items,
        types::{self as analyzer_types, Type},
    },
};
use fe_common::numeric::Literal;
use fe_parser::{ast, node::Node};
use fxhash::FxHashMap;
use id_arena::{Arena, Id};
use num_bigint::BigInt;
use smol_str::SmolStr;

use crate::{
    db::MirDb,
    ir::{
        self,
        body_builder::BodyBuilder,
        constant::ConstantValue,
        function::Linkage,
        inst::{CallType, InstKind},
        value::{AssignableValue, Local},
        BasicBlockId, Constant, FunctionBody, FunctionId, FunctionParam, FunctionSignature, InstId,
        SourceInfo, TypeId, Value, ValueId,
    },
};

type ScopeId = Id<Scope>;

pub fn lower_func_signature(db: &dyn MirDb, func: analyzer_items::FunctionId) -> FunctionId {
    lower_monomorphized_func_signature(db, func, BTreeMap::new())
}
pub fn lower_monomorphized_func_signature(
    db: &dyn MirDb,
    func: analyzer_items::FunctionId,
    resolved_generics: BTreeMap<SmolStr, analyzer_types::TypeId>,
) -> FunctionId {
    // TODO: Remove this when an analyzer's function signature contains `self` type.
    let mut params = vec![];

    if func.takes_self(db.upcast()) {
        let self_ty = func.self_type(db.upcast()).unwrap();
        let source = self_arg_source(db, func);
        params.push(make_param(db, "self", self_ty, source));
    }
    let analyzer_signature = func.signature(db.upcast());

    for param in analyzer_signature.params.iter() {
        let source = arg_source(db, func, &param.name);

        let param_type =
            if let Type::Generic(generic) = param.typ.clone().unwrap().deref_typ(db.upcast()) {
                *resolved_generics.get(&generic.name).unwrap()
            } else {
                param.typ.clone().unwrap()
            };

        params.push(make_param(db, param.clone().name, param_type, source))
    }

    let return_type = db.mir_lowered_type(analyzer_signature.return_type.clone().unwrap());

    let linkage = if func.is_public(db.upcast()) {
        if func.is_contract_func(db.upcast()) && !func.is_constructor(db.upcast()) {
            Linkage::Export
        } else {
            Linkage::Public
        }
    } else {
        Linkage::Private
    };

    let sig = FunctionSignature {
        params,
        resolved_generics,
        return_type: Some(return_type),
        module_id: func.module(db.upcast()),
        analyzer_func_id: func,
        linkage,
    };

    db.mir_intern_function(sig.into())
}

pub fn lower_func_body(db: &dyn MirDb, func: FunctionId) -> Rc<FunctionBody> {
    let analyzer_func = func.analyzer_func(db);
    let ast = &analyzer_func.data(db.upcast()).ast;
    let analyzer_body = analyzer_func.body(db.upcast());

    BodyLowerHelper::new(db, func, ast, analyzer_body.as_ref())
        .lower()
        .into()
}

pub(super) struct BodyLowerHelper<'db, 'a> {
    pub(super) db: &'db dyn MirDb,
    pub(super) builder: BodyBuilder,
    ast: &'a Node<ast::Function>,
    func: FunctionId,
    analyzer_body: &'a fe_analyzer::context::FunctionBody,
    scopes: Arena<Scope>,
    current_scope: ScopeId,
}

impl<'db, 'a> BodyLowerHelper<'db, 'a> {
    pub(super) fn lower_stmt(&mut self, stmt: &Node<ast::FuncStmt>) {
        match &stmt.kind {
            ast::FuncStmt::Return { value } => {
                let value = if let Some(expr) = value {
                    self.lower_expr_to_value(expr)
                } else {
                    self.make_unit()
                };
                self.builder.ret(value, stmt.into());
                let next_block = self.builder.make_block();
                self.builder.move_to_block(next_block);
            }

            ast::FuncStmt::VarDecl { target, value, .. } => {
                self.lower_var_decl(target, value.as_ref(), stmt.into());
            }

            ast::FuncStmt::ConstantDecl { name, value, .. } => {
                let ty = self.lower_analyzer_type(self.analyzer_body.var_types[&name.id]);

                let value = self.analyzer_body.expressions[&value.id]
                    .const_value
                    .clone()
                    .unwrap();

                let constant =
                    self.make_local_constant(name.kind.clone(), ty, value.into(), stmt.into());
                self.scope_mut().declare_var(&name.kind, constant);
            }

            ast::FuncStmt::Assign { target, value } => {
                let result = self.lower_assignable_value(target);
                let (expr, _ty) = self.lower_expr(value);
                self.builder.map_result(expr, result)
            }

            ast::FuncStmt::AugAssign { target, op, value } => {
                let result = self.lower_assignable_value(target);
                let lhs = self.lower_expr_to_value(target);
                let rhs = self.lower_expr_to_value(value);

                let inst = self.lower_binop(op.kind, lhs, rhs, stmt.into());
                self.builder.map_result(inst, result)
            }

            ast::FuncStmt::For { target, iter, body } => self.lower_for_loop(target, iter, body),

            ast::FuncStmt::While { test, body } => {
                let header_bb = self.builder.make_block();
                let exit_bb = self.builder.make_block();

                let cond = self.lower_expr_to_value(test);
                self.builder
                    .branch(cond, header_bb, exit_bb, SourceInfo::dummy());

                // Lower while body.
                self.builder.move_to_block(header_bb);
                self.enter_loop_scope(header_bb, exit_bb);
                for stmt in body {
                    self.lower_stmt(stmt);
                }
                let cond = self.lower_expr_to_value(test);
                self.builder
                    .branch(cond, header_bb, exit_bb, SourceInfo::dummy());

                self.leave_scope();

                // Move to while exit bb.
                self.builder.move_to_block(exit_bb);
            }

            ast::FuncStmt::If {
                test,
                body,
                or_else,
            } => self.lower_if(test, body, or_else),

            ast::FuncStmt::Match { expr, arms } => {
                let matrix = &self.analyzer_body.matches[&stmt.id];
                super::pattern_match::lower_match(self, matrix, expr, arms);
            }

            ast::FuncStmt::Assert { test, msg } => {
                let then_bb = self.builder.make_block();
                let false_bb = self.builder.make_block();

                let cond = self.lower_expr_to_value(test);
                self.builder
                    .branch(cond, then_bb, false_bb, SourceInfo::dummy());

                self.builder.move_to_block(false_bb);

                let msg = match msg {
                    Some(msg) => self.lower_expr_to_value(msg),
                    None => self.make_u256_imm(1),
                };
                self.builder.revert(Some(msg), stmt.into());
                self.builder.move_to_block(then_bb);
            }

            ast::FuncStmt::Expr { value } => {
                self.lower_expr_to_value(value);
            }

            ast::FuncStmt::Break => {
                let exit = self.scope().loop_exit(&self.scopes);
                self.builder.jump(exit, stmt.into());
                let next_block = self.builder.make_block();
                self.builder.move_to_block(next_block);
            }

            ast::FuncStmt::Continue => {
                let entry = self.scope().loop_entry(&self.scopes);
                if let Some(loop_idx) = self.scope().loop_idx(&self.scopes) {
                    let imm_one = self.make_u256_imm(1u32);
                    let inc = self.builder.add(loop_idx, imm_one, SourceInfo::dummy());
                    self.builder.map_result(inc, loop_idx.into());
                    let maximum_iter_count = self.scope().maximum_iter_count(&self.scopes).unwrap();
                    let exit = self.scope().loop_exit(&self.scopes);
                    self.branch_eq(loop_idx, maximum_iter_count, exit, entry, stmt.into());
                } else {
                    self.builder.jump(entry, stmt.into());
                }
                let next_block = self.builder.make_block();
                self.builder.move_to_block(next_block);
            }

            ast::FuncStmt::Revert { error } => {
                let error = error.as_ref().map(|err| self.lower_expr_to_value(err));
                self.builder.revert(error, stmt.into());
                let next_block = self.builder.make_block();
                self.builder.move_to_block(next_block);
            }

            ast::FuncStmt::Unsafe(stmts) => {
                self.enter_scope();
                for stmt in stmts {
                    self.lower_stmt(stmt)
                }
                self.leave_scope()
            }
        }
    }

    pub(super) fn lower_var_decl(
        &mut self,
        var: &Node<ast::VarDeclTarget>,
        init: Option<&Node<ast::Expr>>,
        source: SourceInfo,
    ) {
        match &var.kind {
            ast::VarDeclTarget::Name(name) => {
                let ty = self.lower_analyzer_type(self.analyzer_body.var_types[&var.id]);
                let value = self.declare_var(name, ty, var.into());
                if let Some(init) = init {
                    let (init, _init_ty) = self.lower_expr(init);
                    // debug_assert_eq!(ty.deref(self.db), init_ty, "vardecl init type mismatch: {}
                    // != {}",                  ty.as_string(self.db),
                    //                  init_ty.as_string(self.db));
                    self.builder.map_result(init, value.into());
                }
            }

            ast::VarDeclTarget::Tuple(decls) => {
                if let Some(init) = init {
                    if let ast::Expr::Tuple { elts } = &init.kind {
                        debug_assert_eq!(decls.len(), elts.len());
                        for (decl, init_elem) in decls.iter().zip(elts.iter()) {
                            self.lower_var_decl(decl, Some(init_elem), source.clone());
                        }
                    } else {
                        let init_ty = self.expr_ty(init);
                        let init_value = self.lower_expr_to_value(init);
                        self.lower_var_decl_unpack(var, init_value, init_ty, source);
                    };
                } else {
                    for decl in decls {
                        self.lower_var_decl(decl, None, source.clone())
                    }
                }
            }
        }
    }

    pub(super) fn declare_var(
        &mut self,
        name: &SmolStr,
        ty: TypeId,
        source: SourceInfo,
    ) -> ValueId {
        let local = Local::user_local(name.clone(), ty, source);
        let value = self.builder.declare(local);
        self.scope_mut().declare_var(name, value);
        value
    }

    pub(super) fn lower_var_decl_unpack(
        &mut self,
        var: &Node<ast::VarDeclTarget>,
        init: ValueId,
        init_ty: TypeId,
        source: SourceInfo,
    ) {
        match &var.kind {
            ast::VarDeclTarget::Name(name) => {
                let ty = self.lower_analyzer_type(self.analyzer_body.var_types[&var.id]);
                let local = Local::user_local(name.clone(), ty, var.into());

                let lhs = self.builder.declare(local);
                self.scope_mut().declare_var(name, lhs);
                let bind = self.builder.bind(init, source);
                self.builder.map_result(bind, lhs.into());
            }

            ast::VarDeclTarget::Tuple(decls) => {
                for (index, decl) in decls.iter().enumerate() {
                    let elem_ty = init_ty.projection_ty_imm(self.db, index);
                    let index_value = self.make_u256_imm(index);
                    let elem_inst =
                        self.builder
                            .aggregate_access(init, vec![index_value], source.clone());
                    let elem_value = self.map_to_tmp(elem_inst, elem_ty);
                    self.lower_var_decl_unpack(decl, elem_value, elem_ty, source.clone())
                }
            }
        }
    }

    pub(super) fn lower_expr(&mut self, expr: &Node<ast::Expr>) -> (InstId, TypeId) {
        let mut ty = self.expr_ty(expr);
        let mut inst = match &expr.kind {
            ast::Expr::Ternary {
                if_expr,
                test,
                else_expr,
            } => {
                let true_bb = self.builder.make_block();
                let false_bb = self.builder.make_block();
                let merge_bb = self.builder.make_block();

                let tmp = self
                    .builder
                    .declare(Local::tmp_local("$ternary_tmp".into(), ty));

                let cond = self.lower_expr_to_value(test);
                self.builder
                    .branch(cond, true_bb, false_bb, SourceInfo::dummy());

                self.builder.move_to_block(true_bb);
                let (value, _) = self.lower_expr(if_expr);
                self.builder.map_result(value, tmp.into());
                self.builder.jump(merge_bb, SourceInfo::dummy());

                self.builder.move_to_block(false_bb);
                let (value, _) = self.lower_expr(else_expr);
                self.builder.map_result(value, tmp.into());
                self.builder.jump(merge_bb, SourceInfo::dummy());

                self.builder.move_to_block(merge_bb);
                self.builder.bind(tmp, SourceInfo::dummy())
            }

            ast::Expr::BoolOperation { left, op, right } => {
                self.lower_bool_op(op.kind, left, right, ty)
            }

            ast::Expr::BinOperation { left, op, right } => {
                let lhs = self.lower_expr_to_value(left);
                let rhs = self.lower_expr_to_value(right);
                self.lower_binop(op.kind, lhs, rhs, expr.into())
            }

            ast::Expr::UnaryOperation { op, operand } => {
                let value = self.lower_expr_to_value(operand);
                match op.kind {
                    ast::UnaryOperator::Invert => self.builder.inv(value, expr.into()),
                    ast::UnaryOperator::Not => self.builder.not(value, expr.into()),
                    ast::UnaryOperator::USub => self.builder.neg(value, expr.into()),
                }
            }

            ast::Expr::CompOperation { left, op, right } => {
                let lhs = self.lower_expr_to_value(left);
                let rhs = self.lower_expr_to_value(right);
                self.lower_comp_op(op.kind, lhs, rhs, expr.into())
            }

            ast::Expr::Attribute { .. } => {
                let mut indices = vec![];
                let value = self.lower_aggregate_access(expr, &mut indices);
                self.builder.aggregate_access(value, indices, expr.into())
            }

            ast::Expr::Subscript { value, index } => {
                let value_ty = self.expr_ty(value).deref(self.db);
                if value_ty.is_aggregate(self.db) {
                    let mut indices = vec![];
                    let value = self.lower_aggregate_access(expr, &mut indices);
                    self.builder.aggregate_access(value, indices, expr.into())
                } else if value_ty.is_map(self.db) {
                    let value = self.lower_expr_to_value(value);
                    let key = self.lower_expr_to_value(index);
                    self.builder.map_access(value, key, expr.into())
                } else {
                    unreachable!()
                }
            }

            ast::Expr::Call {
                func,
                generic_args,
                args,
            } => {
                let ty = self.expr_ty(expr);
                self.lower_call(func, generic_args, &args.kind, ty, expr.into())
            }

            ast::Expr::List { elts } | ast::Expr::Tuple { elts } => {
                let args = elts
                    .iter()
                    .map(|elem| self.lower_expr_to_value(elem))
                    .collect();
                let ty = self.expr_ty(expr);
                self.builder.aggregate_construct(ty, args, expr.into())
            }

            ast::Expr::Repeat { value, len: _ } => {
                let array_type = if let Type::Array(array_type) = self.analyzer_body.expressions
                    [&expr.id]
                    .typ
                    .typ(self.db.upcast())
                {
                    array_type
                } else {
                    panic!("not an array");
                };

                let args = vec![self.lower_expr_to_value(value); array_type.size];
                let ty = self.expr_ty(expr);
                self.builder.aggregate_construct(ty, args, expr.into())
            }

            ast::Expr::Bool(b) => {
                let imm = self.builder.make_imm_from_bool(*b, ty);
                self.builder.bind(imm, expr.into())
            }

            ast::Expr::Name(name) => {
                let value = self.resolve_name(name);
                self.builder.bind(value, expr.into())
            }

            ast::Expr::Path(path) => {
                let value = self.resolve_path(path, expr.into());
                self.builder.bind(value, expr.into())
            }

            ast::Expr::Num(num) => {
                let imm = Literal::new(num).parse().unwrap();
                let imm = self.builder.make_imm(imm, ty);
                self.builder.bind(imm, expr.into())
            }

            ast::Expr::Str(s) => {
                let ty = self.expr_ty(expr);
                let const_value = self.make_local_constant(
                    "str_in_func".into(),
                    ty,
                    ConstantValue::Str(s.clone()),
                    expr.into(),
                );
                self.builder.bind(const_value, expr.into())
            }

            ast::Expr::Unit => {
                let value = self.make_unit();
                self.builder.bind(value, expr.into())
            }
        };

        for Adjustment { into, kind } in &self.analyzer_body.expressions[&expr.id].type_adjustments
        {
            let into_ty = self.lower_analyzer_type(*into);

            match kind {
                AdjustmentKind::Copy => {
                    let val = self.inst_result_or_tmp(inst, ty);
                    inst = self.builder.mem_copy(val, expr.into());
                }
                AdjustmentKind::Load => {
                    let val = self.inst_result_or_tmp(inst, ty);
                    inst = self.builder.load(val, expr.into());
                }
                AdjustmentKind::IntSizeIncrease => {
                    let val = self.inst_result_or_tmp(inst, ty);
                    inst = self.builder.primitive_cast(val, into_ty, expr.into())
                }
                AdjustmentKind::StringSizeIncrease => {} // XXX
            }
            ty = into_ty;
        }
        (inst, ty)
    }

    fn inst_result_or_tmp(&mut self, inst: InstId, ty: TypeId) -> ValueId {
        self.builder
            .inst_result(inst)
            .and_then(|r| r.value_id())
            .unwrap_or_else(|| self.map_to_tmp(inst, ty))
    }

    pub(super) fn lower_expr_to_value(&mut self, expr: &Node<ast::Expr>) -> ValueId {
        let (inst, ty) = self.lower_expr(expr);
        self.map_to_tmp(inst, ty)
    }

    pub(super) fn enter_scope(&mut self) {
        let new_scope = Scope::with_parent(self.current_scope);
        self.current_scope = self.scopes.alloc(new_scope);
    }

    pub(super) fn leave_scope(&mut self) {
        self.current_scope = self.scopes[self.current_scope].parent.unwrap();
    }

    pub(super) fn make_imm(&mut self, imm: impl Into<BigInt>, ty: TypeId) -> ValueId {
        self.builder.make_value(Value::Immediate {
            imm: imm.into(),
            ty,
        })
    }

    pub(super) fn make_u256_imm(&mut self, value: impl Into<BigInt>) -> ValueId {
        let u256_ty = self.u256_ty();
        self.make_imm(value, u256_ty)
    }

    pub(super) fn map_to_tmp(&mut self, inst: InstId, ty: TypeId) -> ValueId {
        match &self.builder.inst_data(inst).kind {
            InstKind::Bind { src } => {
                let value = *src;
                self.builder.remove_inst(inst);
                value
            }
            _ => {
                let tmp = Value::Temporary { inst, ty };
                let result = self.builder.make_value(tmp);
                self.builder.map_result(inst, result.into());
                result
            }
        }
    }

    fn new(
        db: &'db dyn MirDb,
        func: FunctionId,
        ast: &'a Node<ast::Function>,
        analyzer_body: &'a fe_analyzer::context::FunctionBody,
    ) -> Self {
        let mut builder = BodyBuilder::new(func, ast.into());
        let mut scopes = Arena::new();

        // Make a root scope. A root scope collects function parameters and module
        // constants.
        let root = Scope::root(db, func, &mut builder);
        let current_scope = scopes.alloc(root);
        Self {
            db,
            builder,
            ast,
            func,
            analyzer_body,
            scopes,
            current_scope,
        }
    }

    fn lower_analyzer_type(&self, analyzer_ty: analyzer_types::TypeId) -> TypeId {
        // If the analyzer type is generic we first need to resolve it to its concrete
        // type before lowering to a MIR type
        if let analyzer_types::Type::Generic(generic) = analyzer_ty.deref_typ(self.db.upcast()) {
            let resolved_type = self
                .func
                .signature(self.db)
                .resolved_generics
                .get(&generic.name)
                .cloned()
                .expect("expected generic to be resolved");

            return self.db.mir_lowered_type(resolved_type);
        }

        self.db.mir_lowered_type(analyzer_ty)
    }

    fn lower(mut self) -> FunctionBody {
        for stmt in &self.ast.kind.body {
            self.lower_stmt(stmt)
        }

        let last_block = self.builder.current_block();
        if !self.builder.is_block_terminated(last_block) {
            let unit = self.make_unit();
            self.builder.ret(unit, SourceInfo::dummy());
        }

        self.builder.build()
    }

    fn branch_eq(
        &mut self,
        v1: ValueId,
        v2: ValueId,
        true_bb: BasicBlockId,
        false_bb: BasicBlockId,
        source: SourceInfo,
    ) {
        let cond = self.builder.eq(v1, v2, source.clone());
        let bool_ty = self.bool_ty();
        let cond = self.map_to_tmp(cond, bool_ty);
        self.builder.branch(cond, true_bb, false_bb, source);
    }

    fn lower_if(
        &mut self,
        cond: &Node<ast::Expr>,
        then: &[Node<ast::FuncStmt>],
        else_: &[Node<ast::FuncStmt>],
    ) {
        let cond = self.lower_expr_to_value(cond);

        if else_.is_empty() {
            let then_bb = self.builder.make_block();
            let merge_bb = self.builder.make_block();

            self.builder
                .branch(cond, then_bb, merge_bb, SourceInfo::dummy());

            // Lower then block.
            self.builder.move_to_block(then_bb);
            self.enter_scope();
            for stmt in then {
                self.lower_stmt(stmt);
            }
            self.builder.jump(merge_bb, SourceInfo::dummy());
            self.builder.move_to_block(merge_bb);
            self.leave_scope();
        } else {
            let then_bb = self.builder.make_block();
            let else_bb = self.builder.make_block();

            self.builder
                .branch(cond, then_bb, else_bb, SourceInfo::dummy());

            // Lower then block.
            self.builder.move_to_block(then_bb);
            self.enter_scope();
            for stmt in then {
                self.lower_stmt(stmt);
            }
            self.leave_scope();
            let then_block_end_bb = self.builder.current_block();

            // Lower else_block.
            self.builder.move_to_block(else_bb);
            self.enter_scope();
            for stmt in else_ {
                self.lower_stmt(stmt);
            }
            self.leave_scope();
            let else_block_end_bb = self.builder.current_block();

            let merge_bb = self.builder.make_block();
            if !self.builder.is_block_terminated(then_block_end_bb) {
                self.builder.move_to_block(then_block_end_bb);
                self.builder.jump(merge_bb, SourceInfo::dummy());
            }
            if !self.builder.is_block_terminated(else_block_end_bb) {
                self.builder.move_to_block(else_block_end_bb);
                self.builder.jump(merge_bb, SourceInfo::dummy());
            }
            self.builder.move_to_block(merge_bb);
        }
    }

    // NOTE: we assume a type of `iter` is array.
    // TODO: Desugar to `loop` + `match` like rustc in HIR to generate better MIR.
    fn lower_for_loop(
        &mut self,
        loop_variable: &Node<SmolStr>,
        iter: &Node<ast::Expr>,
        body: &[Node<ast::FuncStmt>],
    ) {
        let preheader_bb = self.builder.make_block();
        let entry_bb = self.builder.make_block();
        let exit_bb = self.builder.make_block();

        let iter_elem_ty = self.analyzer_body.var_types[&loop_variable.id];
        let iter_elem_ty = self.lower_analyzer_type(iter_elem_ty);

        self.builder.jump(preheader_bb, SourceInfo::dummy());

        // `For` has its scope from preheader block.
        self.enter_loop_scope(entry_bb, exit_bb);

        /* Lower preheader. */
        self.builder.move_to_block(preheader_bb);

        // Declare loop_variable.
        let loop_value = self.builder.declare(Local::user_local(
            loop_variable.kind.clone(),
            iter_elem_ty,
            loop_variable.into(),
        ));
        self.scope_mut()
            .declare_var(&loop_variable.kind, loop_value);

        // Declare and initialize `loop_idx` to 0.
        let loop_idx = Local::tmp_local("$loop_idx_tmp".into(), self.u256_ty());
        let loop_idx = self.builder.declare(loop_idx);
        let imm_zero = self.make_u256_imm(0u32);
        let imm_zero = self.builder.bind(imm_zero, SourceInfo::dummy());
        self.builder.map_result(imm_zero, loop_idx.into());

        // Evaluates loop variable.
        let iter_ty = self.expr_ty(iter);
        let iter = self.lower_expr_to_value(iter);

        // Create maximum loop count.
        let maximum_iter_count = match &iter_ty.deref(self.db).data(self.db).kind {
            ir::TypeKind::Array(ir::types::ArrayDef { len, .. }) => *len,
            _ => unreachable!(),
        };
        let maximum_iter_count = self.make_u256_imm(maximum_iter_count);
        self.branch_eq(
            loop_idx,
            maximum_iter_count,
            exit_bb,
            entry_bb,
            SourceInfo::dummy(),
        );
        self.scope_mut().loop_idx = Some(loop_idx);
        self.scope_mut().maximum_iter_count = Some(maximum_iter_count);

        /* Lower body. */
        self.builder.move_to_block(entry_bb);

        // loop_variable = array[loop_idx]
        let iter_elem = self
            .builder
            .aggregate_access(iter, vec![loop_idx], SourceInfo::dummy());
        self.builder
            .map_result(iter_elem, AssignableValue::Value(loop_value));

        for stmt in body {
            self.lower_stmt(stmt);
        }

        // loop_idx += 1
        let imm_one = self.make_u256_imm(1u32);
        let inc = self.builder.add(loop_idx, imm_one, SourceInfo::dummy());
        self.builder
            .map_result(inc, AssignableValue::Value(loop_idx));
        self.branch_eq(
            loop_idx,
            maximum_iter_count,
            exit_bb,
            entry_bb,
            SourceInfo::dummy(),
        );

        /* Move to exit bb */
        self.leave_scope();
        self.builder.move_to_block(exit_bb);
    }

    fn lower_assignable_value(&mut self, expr: &Node<ast::Expr>) -> AssignableValue {
        match &expr.kind {
            ast::Expr::Attribute { value, attr } => {
                let idx = self.expr_ty(value).index_from_fname(self.db, &attr.kind);
                let idx = self.make_u256_imm(idx);
                let lhs = self.lower_assignable_value(value).into();
                AssignableValue::Aggregate { lhs, idx }
            }
            ast::Expr::Subscript { value, index } => {
                let lhs = self.lower_assignable_value(value).into();
                let attr = self.lower_expr_to_value(index);
                let value_ty = self.expr_ty(value).deref(self.db);
                if value_ty.is_aggregate(self.db) {
                    AssignableValue::Aggregate { lhs, idx: attr }
                } else if value_ty.is_map(self.db) {
                    AssignableValue::Map { lhs, key: attr }
                } else {
                    unreachable!()
                }
            }
            ast::Expr::Name(name) => self.resolve_name(name).into(),
            ast::Expr::Path(path) => self.resolve_path(path, expr.into()).into(),
            _ => self.lower_expr_to_value(expr).into(),
        }
    }

    /// Returns the pre-adjustment type of the given `Expr`
    fn expr_ty(&self, expr: &Node<ast::Expr>) -> TypeId {
        let analyzer_ty = self.analyzer_body.expressions[&expr.id].typ;
        self.lower_analyzer_type(analyzer_ty)
    }

    fn lower_bool_op(
        &mut self,
        op: ast::BoolOperator,
        lhs: &Node<ast::Expr>,
        rhs: &Node<ast::Expr>,
        ty: TypeId,
    ) -> InstId {
        let true_bb = self.builder.make_block();
        let false_bb = self.builder.make_block();
        let merge_bb = self.builder.make_block();

        let lhs = self.lower_expr_to_value(lhs);
        let tmp = self
            .builder
            .declare(Local::tmp_local(format!("${op}_tmp").into(), ty));

        match op {
            ast::BoolOperator::And => {
                self.builder
                    .branch(lhs, true_bb, false_bb, SourceInfo::dummy());

                self.builder.move_to_block(true_bb);
                let (rhs, _rhs_ty) = self.lower_expr(rhs);
                self.builder.map_result(rhs, tmp.into());
                self.builder.jump(merge_bb, SourceInfo::dummy());

                self.builder.move_to_block(false_bb);
                let false_imm = self.builder.make_imm_from_bool(false, ty);
                let false_imm_copy = self.builder.bind(false_imm, SourceInfo::dummy());
                self.builder.map_result(false_imm_copy, tmp.into());
                self.builder.jump(merge_bb, SourceInfo::dummy());
            }

            ast::BoolOperator::Or => {
                self.builder
                    .branch(lhs, true_bb, false_bb, SourceInfo::dummy());

                self.builder.move_to_block(true_bb);
                let true_imm = self.builder.make_imm_from_bool(true, ty);
                let true_imm_copy = self.builder.bind(true_imm, SourceInfo::dummy());
                self.builder.map_result(true_imm_copy, tmp.into());
                self.builder.jump(merge_bb, SourceInfo::dummy());

                self.builder.move_to_block(false_bb);
                let (rhs, _rhs_ty) = self.lower_expr(rhs);
                self.builder.map_result(rhs, tmp.into());
                self.builder.jump(merge_bb, SourceInfo::dummy());
            }
        }

        self.builder.move_to_block(merge_bb);
        self.builder.bind(tmp, SourceInfo::dummy())
    }

    fn lower_binop(
        &mut self,
        op: ast::BinOperator,
        lhs: ValueId,
        rhs: ValueId,
        source: SourceInfo,
    ) -> InstId {
        match op {
            ast::BinOperator::Add => self.builder.add(lhs, rhs, source),
            ast::BinOperator::Sub => self.builder.sub(lhs, rhs, source),
            ast::BinOperator::Mult => self.builder.mul(lhs, rhs, source),
            ast::BinOperator::Div => self.builder.div(lhs, rhs, source),
            ast::BinOperator::Mod => self.builder.modulo(lhs, rhs, source),
            ast::BinOperator::Pow => self.builder.pow(lhs, rhs, source),
            ast::BinOperator::LShift => self.builder.shl(lhs, rhs, source),
            ast::BinOperator::RShift => self.builder.shr(lhs, rhs, source),
            ast::BinOperator::BitOr => self.builder.bit_or(lhs, rhs, source),
            ast::BinOperator::BitXor => self.builder.bit_xor(lhs, rhs, source),
            ast::BinOperator::BitAnd => self.builder.bit_and(lhs, rhs, source),
        }
    }

    fn lower_comp_op(
        &mut self,
        op: ast::CompOperator,
        lhs: ValueId,
        rhs: ValueId,
        source: SourceInfo,
    ) -> InstId {
        match op {
            ast::CompOperator::Eq => self.builder.eq(lhs, rhs, source),
            ast::CompOperator::NotEq => self.builder.ne(lhs, rhs, source),
            ast::CompOperator::Lt => self.builder.lt(lhs, rhs, source),
            ast::CompOperator::LtE => self.builder.le(lhs, rhs, source),
            ast::CompOperator::Gt => self.builder.gt(lhs, rhs, source),
            ast::CompOperator::GtE => self.builder.ge(lhs, rhs, source),
        }
    }

    fn resolve_generics_args(
        &mut self,
        method: &analyzer_items::FunctionId,
        args: &[Id<Value>],
    ) -> BTreeMap<SmolStr, analyzer_types::TypeId> {
        method
            .signature(self.db.upcast())
            .params
            .iter()
            .zip(args.iter().map(|val| {
                self.builder
                    .value_ty(*val)
                    .analyzer_ty(self.db)
                    .expect("invalid parameter")
            }))
            .filter_map(|(param, typ)| {
                if let Type::Generic(generic) =
                    param.typ.clone().unwrap().deref_typ(self.db.upcast())
                {
                    Some((generic.name, typ))
                } else {
                    None
                }
            })
            .collect::<BTreeMap<_, _>>()
    }

    fn lower_function_id(
        &mut self,
        function: &analyzer_items::FunctionId,
        args: &[Id<Value>],
    ) -> FunctionId {
        let resolved_generics = self.resolve_generics_args(function, args);
        if function.is_generic(self.db.upcast()) {
            self.db
                .mir_lowered_monomorphized_func_signature(*function, resolved_generics)
        } else {
            self.db.mir_lowered_func_signature(*function)
        }
    }

    fn lower_call(
        &mut self,
        func: &Node<ast::Expr>,
        _generic_args: &Option<Node<Vec<ast::GenericArg>>>,
        args: &[Node<ast::CallArg>],
        ty: TypeId,
        source: SourceInfo,
    ) -> InstId {
        let call_type = &self.analyzer_body.calls[&func.id];

        let mut args: Vec<_> = args
            .iter()
            .map(|arg| self.lower_expr_to_value(&arg.kind.value))
            .collect();

        match call_type {
            AnalyzerCallType::BuiltinFunction(GlobalFunction::Keccak256) => {
                self.builder.keccak256(args[0], source)
            }

            AnalyzerCallType::Intrinsic(intrinsic) => {
                self.builder
                    .yul_intrinsic((*intrinsic).into(), args, source)
            }

            AnalyzerCallType::BuiltinValueMethod { method, .. } => {
                let arg = self.lower_method_receiver(func);
                match method {
                    ValueMethod::ToMem => self.builder.mem_copy(arg, source),
                    ValueMethod::AbiEncode => self.builder.abi_encode(arg, source),
                }
            }

            // We ignores `args[0]', which represents `context` and not used for now.
            AnalyzerCallType::BuiltinAssociatedFunction { contract, function } => match function {
                ContractTypeMethod::Create => self.builder.create(args[1], *contract, source),
                ContractTypeMethod::Create2 => {
                    self.builder.create2(args[1], args[2], *contract, source)
                }
            },

            AnalyzerCallType::AssociatedFunction { function, .. }
            | AnalyzerCallType::Pure(function) => {
                let func_id = self.lower_function_id(function, &args);
                self.builder.call(func_id, args, CallType::Internal, source)
            }

            AnalyzerCallType::ValueMethod { method, .. } => {
                let mut method_args = vec![self.lower_method_receiver(func)];
                let func_id = self.lower_function_id(method, &args);

                method_args.append(&mut args);

                self.builder
                    .call(func_id, method_args, CallType::Internal, source)
            }
            AnalyzerCallType::TraitValueMethod {
                trait_id, method, ..
            } if trait_id.is_std_trait(self.db.upcast(), EMITTABLE_TRAIT_NAME)
                && method.name(self.db.upcast()) == EMIT_FN_NAME =>
            {
                let event = self.lower_method_receiver(func);
                self.builder.emit(event, source)
            }
            AnalyzerCallType::TraitValueMethod {
                method,
                trait_id,
                generic_type,
                ..
            } => {
                let mut method_args = vec![self.lower_method_receiver(func)];
                method_args.append(&mut args);

                let concrete_type = self
                    .func
                    .signature(self.db)
                    .resolved_generics
                    .get(&generic_type.name)
                    .cloned()
                    .expect("unresolved generic type");

                let impl_ = concrete_type
                    .get_impl_for(self.db.upcast(), *trait_id)
                    .expect("missing impl");

                let function = impl_
                    .function(self.db.upcast(), &method.name(self.db.upcast()))
                    .expect("missing function");

                let func_id = self.db.mir_lowered_func_signature(function);
                self.builder
                    .call(func_id, method_args, CallType::Internal, source)
            }
            AnalyzerCallType::External { function, .. } => {
                let receiver = self.lower_method_receiver(func);
                debug_assert!(self.builder.value_ty(receiver).is_address(self.db));

                let mut method_args = vec![receiver];
                method_args.append(&mut args);
                let func_id = self.db.mir_lowered_func_signature(*function);
                self.builder
                    .call(func_id, method_args, CallType::External, source)
            }

            AnalyzerCallType::TypeConstructor(to_ty) => {
                if to_ty.is_string(self.db.upcast()) {
                    let arg = *args.last().unwrap();
                    self.builder.mem_copy(arg, source)
                } else if ty.is_primitive(self.db) {
                    // TODO: Ignore `ctx` for now.
                    let arg = *args.last().unwrap();
                    let arg_ty = self.builder.value_ty(arg);
                    if arg_ty == ty {
                        self.builder.bind(arg, source)
                    } else {
                        debug_assert!(!arg_ty.is_ptr(self.db)); // Should be explicitly `Load`ed
                        self.builder.primitive_cast(arg, ty, source)
                    }
                } else if ty.is_aggregate(self.db) {
                    self.builder.aggregate_construct(ty, args, source)
                } else {
                    unreachable!()
                }
            }

            AnalyzerCallType::EnumConstructor(variant) => {
                let tag_type = ty.enum_disc_type(self.db);
                let tag = self.make_imm(variant.disc(self.db.upcast()), tag_type);
                let data_ty = ty.enum_variant_type(self.db, *variant);
                let enum_args = if data_ty.is_unit(self.db) {
                    vec![tag, self.make_unit()]
                } else {
                    std::iter::once(tag).chain(args).collect()
                };
                self.builder.aggregate_construct(ty, enum_args, source)
            }
        }
    }

    // FIXME: This is ugly hack to properly analyze method call. Remove this when  https://github.com/ethereum/fe/issues/670 is resolved.
    fn lower_method_receiver(&mut self, receiver: &Node<ast::Expr>) -> ValueId {
        match &receiver.kind {
            ast::Expr::Attribute { value, .. } => self.lower_expr_to_value(value),
            _ => unreachable!(),
        }
    }

    fn lower_aggregate_access(
        &mut self,
        expr: &Node<ast::Expr>,
        indices: &mut Vec<ValueId>,
    ) -> ValueId {
        match &expr.kind {
            ast::Expr::Attribute { value, attr } => {
                let index = self.expr_ty(value).index_from_fname(self.db, &attr.kind);
                let value = self.lower_aggregate_access(value, indices);
                indices.push(self.make_u256_imm(index));
                value
            }

            ast::Expr::Subscript { value, index }
                if self.expr_ty(value).deref(self.db).is_aggregate(self.db) =>
            {
                let value = self.lower_aggregate_access(value, indices);
                indices.push(self.lower_expr_to_value(index));
                value
            }

            _ => self.lower_expr_to_value(expr),
        }
    }

    fn make_unit(&mut self) -> ValueId {
        let unit_ty = analyzer_types::TypeId::unit(self.db.upcast());
        let unit_ty = self.db.mir_lowered_type(unit_ty);
        self.builder.make_unit(unit_ty)
    }

    fn make_local_constant(
        &mut self,
        name: SmolStr,
        ty: TypeId,
        value: ConstantValue,
        source: SourceInfo,
    ) -> ValueId {
        let function_id = self.builder.func_id();
        let constant = Constant {
            name,
            value,
            ty,
            module_id: function_id.module(self.db),
            source,
        };

        let constant_id = self.db.mir_intern_const(constant.into());
        self.builder.make_constant(constant_id, ty)
    }

    fn u256_ty(&mut self) -> TypeId {
        self.db
            .mir_intern_type(ir::Type::new(ir::TypeKind::U256, None).into())
    }

    fn bool_ty(&mut self) -> TypeId {
        self.db
            .mir_intern_type(ir::Type::new(ir::TypeKind::Bool, None).into())
    }

    fn enter_loop_scope(&mut self, entry: BasicBlockId, exit: BasicBlockId) {
        let new_scope = Scope::loop_scope(self.current_scope, entry, exit);
        self.current_scope = self.scopes.alloc(new_scope);
    }

    /// Resolve a name appeared in an expression.
    /// NOTE: Don't call this to resolve method receiver.
    fn resolve_name(&mut self, name: &str) -> ValueId {
        if let Some(value) = self.scopes[self.current_scope].resolve_name(&self.scopes, name) {
            // Name is defined in local.
            value
        } else {
            // Name is defined in global.
            let func_id = self.builder.func_id();
            let module = func_id.module(self.db);
            let constant = match module
                .resolve_name(self.db.upcast(), name)
                .unwrap()
                .unwrap()
            {
                NamedThing::Item(analyzer_items::Item::Constant(id)) => {
                    self.db.mir_lowered_constant(id)
                }
                _ => panic!("name defined in global must be constant"),
            };
            let ty = constant.ty(self.db);
            self.builder.make_constant(constant, ty)
        }
    }

    /// Resolve a path appeared in an expression.
    /// NOTE: Don't call this to resolve method receiver.
    fn resolve_path(&mut self, path: &ast::Path, source: SourceInfo) -> ValueId {
        let func_id = self.builder.func_id();
        let module = func_id.module(self.db);
        match module.resolve_path(self.db.upcast(), path).value.unwrap() {
            NamedThing::Item(analyzer_items::Item::Constant(id)) => {
                let constant = self.db.mir_lowered_constant(id);
                let ty = constant.ty(self.db);
                self.builder.make_constant(constant, ty)
            }
            NamedThing::EnumVariant(variant) => {
                let enum_ty = self
                    .db
                    .mir_lowered_type(variant.parent(self.db.upcast()).as_type(self.db.upcast()));
                let tag_type = enum_ty.enum_disc_type(self.db);
                let tag = self.make_imm(variant.disc(self.db.upcast()), tag_type);
                let data = self.make_unit();
                let enum_args = vec![tag, data];
                let inst = self.builder.aggregate_construct(enum_ty, enum_args, source);
                self.map_to_tmp(inst, enum_ty)
            }
            _ => panic!("path defined in global must be constant"),
        }
    }

    fn scope(&self) -> &Scope {
        &self.scopes[self.current_scope]
    }

    fn scope_mut(&mut self) -> &mut Scope {
        &mut self.scopes[self.current_scope]
    }
}

#[derive(Debug)]
struct Scope {
    parent: Option<ScopeId>,
    loop_entry: Option<BasicBlockId>,
    loop_exit: Option<BasicBlockId>,
    variables: FxHashMap<SmolStr, ValueId>,
    // TODO: Remove the below two fields when `for` loop desugaring is implemented.
    loop_idx: Option<ValueId>,
    maximum_iter_count: Option<ValueId>,
}

impl Scope {
    fn root(db: &dyn MirDb, func: FunctionId, builder: &mut BodyBuilder) -> Self {
        let mut root = Self {
            parent: None,
            loop_entry: None,
            loop_exit: None,
            variables: FxHashMap::default(),
            loop_idx: None,
            maximum_iter_count: None,
        };

        // Declare function parameters.
        for param in &func.signature(db).params {
            let local = Local::arg_local(param.name.clone(), param.ty, param.source.clone());
            let value_id = builder.store_func_arg(local);
            root.declare_var(&param.name, value_id)
        }

        root
    }

    fn with_parent(parent: ScopeId) -> Self {
        Self {
            parent: parent.into(),
            loop_entry: None,
            loop_exit: None,
            variables: FxHashMap::default(),
            loop_idx: None,
            maximum_iter_count: None,
        }
    }

    fn loop_scope(parent: ScopeId, loop_entry: BasicBlockId, loop_exit: BasicBlockId) -> Self {
        Self {
            parent: parent.into(),
            loop_entry: loop_entry.into(),
            loop_exit: loop_exit.into(),
            variables: FxHashMap::default(),
            loop_idx: None,
            maximum_iter_count: None,
        }
    }

    fn loop_entry(&self, scopes: &Arena<Scope>) -> BasicBlockId {
        match self.loop_entry {
            Some(entry) => entry,
            None => scopes[self.parent.unwrap()].loop_entry(scopes),
        }
    }

    fn loop_exit(&self, scopes: &Arena<Scope>) -> BasicBlockId {
        match self.loop_exit {
            Some(exit) => exit,
            None => scopes[self.parent.unwrap()].loop_exit(scopes),
        }
    }

    fn loop_idx(&self, scopes: &Arena<Scope>) -> Option<ValueId> {
        match self.loop_idx {
            Some(idx) => Some(idx),
            None => scopes[self.parent?].loop_idx(scopes),
        }
    }

    fn maximum_iter_count(&self, scopes: &Arena<Scope>) -> Option<ValueId> {
        match self.maximum_iter_count {
            Some(count) => Some(count),
            None => scopes[self.parent?].maximum_iter_count(scopes),
        }
    }

    fn declare_var(&mut self, name: &SmolStr, value: ValueId) {
        debug_assert!(!self.variables.contains_key(name));

        self.variables.insert(name.clone(), value);
    }

    fn resolve_name(&self, scopes: &Arena<Scope>, name: &str) -> Option<ValueId> {
        match self.variables.get(name) {
            Some(id) => Some(*id),
            None => scopes[self.parent?].resolve_name(scopes, name),
        }
    }
}

fn self_arg_source(db: &dyn MirDb, func: analyzer_items::FunctionId) -> SourceInfo {
    func.data(db.upcast())
        .ast
        .kind
        .sig
        .kind
        .args
        .iter()
        .find(|arg| matches!(arg.kind, ast::FunctionArg::Self_ { .. }))
        .unwrap()
        .into()
}

fn arg_source(db: &dyn MirDb, func: analyzer_items::FunctionId, arg_name: &str) -> SourceInfo {
    func.data(db.upcast())
        .ast
        .kind
        .sig
        .kind
        .args
        .iter()
        .find_map(|arg| match &arg.kind {
            ast::FunctionArg::Regular { name, .. } => {
                if name.kind == arg_name {
                    Some(name.into())
                } else {
                    None
                }
            }
            ast::FunctionArg::Self_ { .. } => None,
        })
        .unwrap()
}

fn make_param(
    db: &dyn MirDb,
    name: impl Into<SmolStr>,
    ty: analyzer_types::TypeId,
    source: SourceInfo,
) -> FunctionParam {
    FunctionParam {
        name: name.into(),
        ty: db.mir_lowered_type(ty),
        source,
    }
}
