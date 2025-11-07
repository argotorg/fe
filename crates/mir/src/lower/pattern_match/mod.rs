use fe_analyzer::pattern_analysis::{ConstructorKind, PatternMatrix};
use fe_parser::{
    ast::{Expr, LiteralPattern, MatchArm},
    node::Node,
};
use fxhash::FxHashMap;
use id_arena::{Arena, Id};
use smol_str::SmolStr;

use crate::ir::{
    body_builder::BodyBuilder, inst::SwitchTable, BasicBlockId, SourceInfo, TypeId, ValueId,
};

use self::decision_tree::{
    Case, ColumnSelectionPolicy, DecisionTree, LeafNode, Occurrence, SwitchNode,
};

use super::function::BodyLowerHelper;

pub mod decision_tree;
mod tree_vis;

pub(super) fn lower_match<'b>(
    helper: &'b mut BodyLowerHelper<'_, '_>,
    mat: &PatternMatrix,
    scrutinee: &Node<Expr>,
    arms: &'b [Node<MatchArm>],
) {
    let mut policy = ColumnSelectionPolicy::default();
    // PBA heuristics described in the paper.
    policy.needed_prefix().small_branching().arity();

    let scrutinee = helper.lower_expr_to_value(scrutinee);
    let decision_tree = decision_tree::build_decision_tree(helper.db.upcast(), mat, policy);

    DecisionTreeLowerHelper::new(helper, scrutinee, arms).lower(decision_tree);
}

struct DecisionTreeLowerHelper<'db, 'a, 'b> {
    helper: &'b mut BodyLowerHelper<'db, 'a>,
    scopes: Arena<Scope>,
    current_scope: ScopeId,
    root_block: BasicBlockId,
    declared_vars: FxHashMap<(SmolStr, usize), ValueId>,
    arms: &'b [Node<MatchArm>],
    lowered_arms: FxHashMap<usize, BasicBlockId>,
    match_exit: BasicBlockId,
}

impl<'db, 'a, 'b> DecisionTreeLowerHelper<'db, 'a, 'b> {
    fn new(
        helper: &'b mut BodyLowerHelper<'db, 'a>,
        scrutinee: ValueId,
        arms: &'b [Node<MatchArm>],
    ) -> Self {
        let match_exit = helper.builder.make_block();

        let mut scope = Scope::default();
        scope.register_occurrence(Occurrence::new(), scrutinee);
        let mut scopes = Arena::new();
        let current_scope = scopes.alloc(scope);

        let root_block = helper.builder.current_block();

        DecisionTreeLowerHelper {
            helper,
            scopes,
            current_scope,
            root_block,
            declared_vars: FxHashMap::default(),
            arms,
            lowered_arms: FxHashMap::default(),
            match_exit,
        }
    }

    fn lower(&mut self, tree: DecisionTree) {
        self.lower_tree(tree);

        let match_exit = self.match_exit;
        self.builder().move_to_block(match_exit);
    }

    fn lower_tree(&mut self, tree: DecisionTree) {
        match tree {
            DecisionTree::Leaf(leaf) => self.lower_leaf(leaf),
            DecisionTree::Switch(switch) => self.lower_switch(switch),
        }
    }

    fn lower_leaf(&mut self, leaf: LeafNode) {
        for (var, occurrence) in leaf.binds {
            let occurrence_value = self.resolve_occurrence(&occurrence);
            let ty = self.builder().value_ty(occurrence_value);
            let var_value = self.declare_or_use_var(&var, ty);

            let inst = self.builder().bind(occurrence_value, SourceInfo::dummy());
            self.builder().map_result(inst, var_value.into());
        }

        let arm_body = self.lower_arm_body(leaf.arm_idx);
        self.builder().jump(arm_body, SourceInfo::dummy());
    }

    fn lower_switch(&mut self, mut switch: SwitchNode) {
        let current_bb = self.builder().current_block();
        let occurrence_value = self.resolve_occurrence(&switch.occurrence);

        if switch.arms.len() == 1 {
            let arm = switch.arms.pop().unwrap();
            let arm_bb = self.enter_arm(&switch.occurrence, &arm.0);
            self.lower_tree(arm.1);
            self.builder().move_to_block(current_bb);
            self.builder().jump(arm_bb, SourceInfo::dummy());
            return;
        }

        let mut table = SwitchTable::default();
        let mut default_arm = None;
        let occurrence_ty = self.builder().value_ty(occurrence_value);

        for (case, tree) in switch.arms {
            let arm_bb = self.enter_arm(&switch.occurrence, &case);
            self.lower_tree(tree);
            self.leave_arm();

            if let Some(disc) = self.case_to_disc(&case, occurrence_ty) {
                table.add_arm(disc, arm_bb);
            } else {
                debug_assert!(default_arm.is_none());
                default_arm = Some(arm_bb);
            }
        }

        self.builder().move_to_block(current_bb);
        let disc = self.extract_disc(occurrence_value);
        self.builder()
            .switch(disc, table, default_arm, SourceInfo::dummy());
    }

    fn lower_arm_body(&mut self, index: usize) -> BasicBlockId {
        if let Some(block) = self.lowered_arms.get(&index) {
            *block
        } else {
            let current_bb = self.builder().current_block();
            let body_bb = self.builder().make_block();

            self.builder().move_to_block(body_bb);
            for stmt in &self.arms[index].kind.body {
                self.helper.lower_stmt(stmt);
            }

            if !self.builder().is_current_block_terminated() {
                let match_exit = self.match_exit;
                self.builder().jump(match_exit, SourceInfo::dummy());
            }

            self.lowered_arms.insert(index, body_bb);
            self.builder().move_to_block(current_bb);
            body_bb
        }
    }

    fn enter_arm(&mut self, occurrence: &Occurrence, case: &Case) -> BasicBlockId {
        self.helper.enter_scope();

        let bb = self.builder().make_block();
        self.builder().move_to_block(bb);

        let scope = Scope::with_parent(self.current_scope);
        self.current_scope = self.scopes.alloc(scope);

        self.update_occurrence(occurrence, case);
        bb
    }

    fn leave_arm(&mut self) {
        self.current_scope = self.scopes[self.current_scope].parent.unwrap();
        self.helper.leave_scope();
    }

    fn case_to_disc(&mut self, case: &Case, occurrence_ty: TypeId) -> Option<ValueId> {
        match case {
            Case::Ctor(ConstructorKind::Enum(variant)) => {
                let disc_ty = occurrence_ty.enum_disc_type(self.helper.db);
                let disc = variant.disc(self.helper.db.upcast());
                Some(self.helper.make_imm(disc, disc_ty))
            }

            Case::Ctor(ConstructorKind::Literal((LiteralPattern::Bool(b), ty))) => {
                let ty = self.helper.db.mir_lowered_type(*ty);
                Some(self.builder().make_imm_from_bool(*b, ty))
            }

            Case::Ctor(ConstructorKind::Tuple(_))
            | Case::Ctor(ConstructorKind::Struct(_))
            | Case::Default => None,
        }
    }

    fn update_occurrence(&mut self, occurrence: &Occurrence, case: &Case) {
        let old_value = self.resolve_occurrence(occurrence);
        let old_ty = self.builder().value_ty(old_value);

        match case {
            Case::Ctor(ConstructorKind::Enum(variant)) => {
                let new_ty = old_ty.enum_variant_type(self.helper.db, *variant);
                let cast = self
                    .builder()
                    .untag_cast(old_value, new_ty, SourceInfo::dummy());
                let value = self.helper.map_to_tmp(cast, new_ty);
                self.current_scope_mut()
                    .register_occurrence(occurrence.clone(), value)
            }

            Case::Ctor(ConstructorKind::Literal((LiteralPattern::Bool(b), _))) => {
                let value = self.builder().make_imm_from_bool(*b, old_ty);
                self.current_scope_mut()
                    .register_occurrence(occurrence.clone(), value)
            }

            Case::Ctor(ConstructorKind::Tuple(_))
            | Case::Ctor(ConstructorKind::Struct(_))
            | Case::Default => {}
        }
    }

    fn extract_disc(&mut self, value: ValueId) -> ValueId {
        let value_ty = self.builder().value_ty(value);
        match value_ty {
            _ if value_ty.deref(self.helper.db).is_enum(self.helper.db) => {
                let disc_ty = value_ty.enum_disc_type(self.helper.db);
                let disc_index = self.helper.make_u256_imm(0);
                let inst =
                    self.builder()
                        .aggregate_access(value, vec![disc_index], SourceInfo::dummy());
                self.helper.map_to_tmp(inst, disc_ty)
            }

            _ => value,
        }
    }

    fn declare_or_use_var(&mut self, var: &(SmolStr, usize), ty: TypeId) -> ValueId {
        if let Some(value) = self.declared_vars.get(var) {
            *value
        } else {
            let current_block = self.builder().current_block();
            let root_block = self.root_block;
            self.builder().move_to_block_top(root_block);
            let value = self.helper.declare_var(&var.0, ty, SourceInfo::dummy());
            self.builder().move_to_block(current_block);
            self.declared_vars.insert(var.clone(), value);
            value
        }
    }

    fn builder(&mut self) -> &mut BodyBuilder {
        &mut self.helper.builder
    }

    fn resolve_occurrence(&mut self, occurrence: &Occurrence) -> ValueId {
        if let Some(value) = self
            .current_scope()
            .resolve_occurrence(&self.scopes, occurrence)
        {
            return value;
        }

        let parent = occurrence.parent().unwrap();
        let parent_value = self.resolve_occurrence(&parent);
        let parent_value_ty = self.builder().value_ty(parent_value);

        let index = occurrence.last_index().unwrap();
        let index_value = self.helper.make_u256_imm(occurrence.last_index().unwrap());
        let inst =
            self.builder()
                .aggregate_access(parent_value, vec![index_value], SourceInfo::dummy());

        let ty = parent_value_ty.projection_ty_imm(self.helper.db, index);
        let value = self.helper.map_to_tmp(inst, ty);
        self.current_scope_mut()
            .register_occurrence(occurrence.clone(), value);
        value
    }

    fn current_scope(&self) -> &Scope {
        self.scopes.get(self.current_scope).unwrap()
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.get_mut(self.current_scope).unwrap()
    }
}

type ScopeId = Id<Scope>;

#[derive(Debug, Default)]
struct Scope {
    parent: Option<ScopeId>,
    occurrences: FxHashMap<Occurrence, ValueId>,
}

impl Scope {
    pub fn with_parent(parent: ScopeId) -> Self {
        Self {
            parent: Some(parent),
            ..Default::default()
        }
    }

    pub fn register_occurrence(&mut self, occurrence: Occurrence, value: ValueId) {
        self.occurrences.insert(occurrence, value);
    }

    pub fn resolve_occurrence(
        &self,
        arena: &Arena<Scope>,
        occurrence: &Occurrence,
    ) -> Option<ValueId> {
        match self.occurrences.get(occurrence) {
            Some(value) => Some(*value),
            None => arena[self.parent?].resolve_occurrence(arena, occurrence),
        }
    }
}
