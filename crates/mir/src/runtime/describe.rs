use std::collections::HashMap;
use common::ir_describe::{DescribeCtx, Dim, IrConsumer, IrDescribe};

use crate::runtime::ir::{
    RExpr, RStmt, RTerminator, RuntimeBody,
    RuntimePlace, PlaceRoot, PlaceElem,
    describe_class, terminator_successors,
};

impl<'db> IrDescribe for RuntimeBody<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        c.enter_node("RuntimeBody");

        c.field_u64(Dim::Structure, self.signature.params.len() as u64);
        for param in &self.signature.params {
            describe_class(cx, c, &param.class);
        }
        if let Some(ret) = &self.signature.ret {
            c.field_bool(Dim::Structure, true);
            describe_class(cx, c, ret);
        }

        c.field_u64(Dim::Structure, self.locals.len() as u64);
        for local in &self.locals {
            match &local.carrier {
                crate::runtime::ir::RuntimeCarrier::Erased => {
                    c.field_u64(Dim::Structure, 0);
                }
                crate::runtime::ir::RuntimeCarrier::Value(class) => {
                    c.field_u64(Dim::Structure, 1);
                    describe_class(cx, c, class);
                }
            }
        }

        // Build def map: which flat stmt index defines which local
        let mut def_map: HashMap<u32, u64> = HashMap::new(); // local_id → flat_stmt_idx
        let mut flat_idx: u64 = 0;
        for block in &self.blocks {
            for stmt in &block.stmts {
                if let RStmt::Assign { dst, .. } = stmt {
                    def_map.insert(dst.as_u32(), flat_idx);
                }
                flat_idx += 1;
            }
            flat_idx += 1; // terminator
        }

        let span_resolver = {
            let db: &dyn crate::db::MirDb = cx.db();
            let key = self.owner.key(db);
            key.semantic(db)
                .and_then(|semantic| semantic.key(db).owner(db).body(db))
                .map(|body| SpanResolver { body })
        };

        flat_idx = 0;
        for (idx, block) in self.blocks.iter().enumerate() {
            c.set_node_id(idx as u64);
            let block_with_edges = BlockWithEdges {
                block,
                terminator: &block.terminator,
                span_resolver,
                def_map: &def_map,
                flat_idx_start: flat_idx,
            };
            c.child(cx, &block_with_edges);
            flat_idx += block.stmts.len() as u64 + 1; // stmts + terminator
        }

        c.exit_node();
    }
}

struct BlockWithEdges<'a, 'db> {
    block: &'a crate::runtime::ir::RBlock<'db>,
    terminator: &'a RTerminator<'db>,
    span_resolver: Option<SpanResolver<'db>>,
    def_map: &'a HashMap<u32, u64>,
    flat_idx_start: u64,
}

#[derive(Clone, Copy)]
struct SpanResolver<'db> {
    body: hir::hir_def::Body<'db>,
}

impl<'db> SpanResolver<'db> {
    fn resolve_and_emit<C: IrConsumer>(
        &self,
        cx: &DescribeCtx<'_>,
        origin: &common::provenance::ProvenanceNodeId,
        c: &mut C,
    ) {
        use common::provenance::IrLevel;
        use hir::span::LazySpan;

        if origin.level != IrLevel::Smir {
            return;
        }

        let db: &dyn crate::db::MirDb = cx.db();
        let expr_id = hir::hir_def::ExprId::from_u32(origin.node);
        let resolved = expr_id.span(self.body).resolve(db);
        if let Some(span) = resolved {
            let start_offset: usize = span.range.start().into();
            let end_offset: usize = span.range.end().into();
            let input_db: &dyn common::InputDb = db.as_dyn_database().as_view();
            let file_path = match span.file.path(input_db) {
                Some(p) => p.to_string(),
                None => String::new(),
            };
            let source_text = span.file.text(db);
            let (start_line, start_col) = common::byte_offset_to_line_col(source_text, start_offset);
            let (end_line, end_col) = common::byte_offset_to_line_col(source_text, end_offset);
            c.source_span(&file_path, start_line as u32, start_col as u32, end_line as u32, end_col as u32);
        }
    }
}

impl<'a, 'db> IrDescribe for BlockWithEdges<'a, 'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        c.enter_node("Block");

        for (i, stmt) in self.block.stmts.iter().enumerate() {
            let stmt_flat_idx = self.flat_idx_start + i as u64;

            // Emit data_flow edges: for each local used by this stmt,
            // look up where it was defined and emit def→use edge
            for used_local in stmt_used_locals(stmt) {
                if let Some(&def_idx) = self.def_map.get(&used_local) {
                    c.data_flow(def_idx, stmt_flat_idx);
                }
            }

            if let Some(origin) = self.block.stmt_origins.get(i) {
                c.origin(origin);
                if let Some(resolver) = &self.span_resolver {
                    resolver.resolve_and_emit(cx, origin, c);
                }
            }
            c.child(cx, stmt);
        }

        let term_flat_idx = self.flat_idx_start + self.block.stmts.len() as u64;
        for used_local in terminator_used_locals(self.terminator) {
            if let Some(&def_idx) = self.def_map.get(&used_local) {
                c.data_flow(def_idx, term_flat_idx);
            }
        }

        if self.block.terminator_origin.level != common::provenance::IrLevel::Mir
            || self.block.terminator_origin.transform != common::provenance::TransformTag::Synthetic
        {
            c.origin(&self.block.terminator_origin);
            if let Some(resolver) = &self.span_resolver {
                resolver.resolve_and_emit(cx, &self.block.terminator_origin, c);
            }
        }
        c.child(cx, self.terminator);

        for (label, target) in terminator_successors(self.terminator) {
            c.graph_edge(label, target.as_u32() as u64);
        }

        c.exit_node();
    }
}

fn stmt_used_locals(stmt: &RStmt<'_>) -> Vec<u32> {
    let mut locals = Vec::new();
    match stmt {
        RStmt::Assign { expr, .. } => expr_used_locals(expr, &mut locals),
        RStmt::EnumAssertVariant { value, .. } => locals.push(value.as_u32()),
        RStmt::Store { dst, src } => {
            place_used_locals(dst, &mut locals);
            locals.push(src.as_u32());
        }
        RStmt::CopyInto { dst, src } => {
            place_used_locals(dst, &mut locals);
            locals.push(src.as_u32());
        }
        RStmt::EnumSetTag { root, .. } => locals.push(root.as_u32()),
        RStmt::EnumWriteVariant { root, fields, .. } => {
            locals.push(root.as_u32());
            for f in fields.iter() { locals.push(f.as_u32()); }
        }
    }
    locals
}

fn expr_used_locals(expr: &RExpr<'_>, out: &mut Vec<u32>) {
    match expr {
        RExpr::Use(v) => out.push(v.as_u32()),
        RExpr::ConstScalar(_) | RExpr::Placeholder { .. }
        | RExpr::ConstRef { .. } | RExpr::AllocObject { .. } => {}
        RExpr::Binary { lhs, rhs, .. } => { out.push(lhs.as_u32()); out.push(rhs.as_u32()); }
        RExpr::Unary { value, .. } => out.push(value.as_u32()),
        RExpr::Cast { value, .. } => out.push(value.as_u32()),
        RExpr::Call { args, .. } => { for a in args.iter() { out.push(a.as_u32()); } }
        RExpr::Load { place } => place_used_locals(place, out),
        RExpr::AggregateExtract { value, .. } => out.push(value.as_u32()),
        RExpr::Builtin(b) => builtin_used_locals(b, out),
        RExpr::MaterializeToObject { src } => out.push(src.as_u32()),
        RExpr::MaterializePlaceToObject { place } => place_used_locals(place, out),
        RExpr::ProviderFromRaw { raw, .. } => out.push(raw.as_u32()),
        RExpr::WordToRawAddr { value, .. } => out.push(value.as_u32()),
        RExpr::ProviderToRaw { value } => out.push(value.as_u32()),
        RExpr::RetagRef { value } => out.push(value.as_u32()),
        RExpr::AddrOf { place } => place_used_locals(place, out),
        RExpr::EnumMake { fields, .. } => { for f in fields.iter() { out.push(f.as_u32()); } }
        RExpr::EnumTagOfValue { value } => out.push(value.as_u32()),
        RExpr::EnumIsVariant { value, .. } => out.push(value.as_u32()),
        RExpr::EnumExtract { value, .. } => out.push(value.as_u32()),
        RExpr::EnumGetTag { root } => out.push(root.as_u32()),
        RExpr::EnumAssertVariantRef { root, .. } => out.push(root.as_u32()),
    }
}

fn place_used_locals(place: &RuntimePlace<'_>, out: &mut Vec<u32>) {
    match &place.root {
        PlaceRoot::Slot(local) => out.push(local.as_u32()),
        PlaceRoot::Ref(value) => out.push(value.as_u32()),
        PlaceRoot::Provider(binding) => out.push(binding.as_u32()),
        PlaceRoot::Ptr { addr, .. } => out.push(addr.as_u32()),
    }
    for elem in place.path.iter() {
        if let PlaceElem::Index(hir::projection::IndexSource::Dynamic(v)) = elem {
            out.push(v.as_u32());
        }
    }
}

fn builtin_used_locals(b: &crate::runtime::ir::RuntimeBuiltin<'_>, out: &mut Vec<u32>) {
    use crate::runtime::ir::RuntimeBuiltin::*;
    match b {
        IntrinsicArith { lhs, rhs, .. } | Saturating { lhs, rhs, .. } => {
            out.push(lhs.as_u32()); out.push(rhs.as_u32());
        }
        Mload { addr } | CallDataLoad { offset: addr } | BlockHash { block: addr }
        | Malloc { size: addr } | Sload { slot: addr } => out.push(addr.as_u32()),
        Mstore { addr, value } | Mstore8 { addr, value } | Sstore { slot: addr, value }
        | SignExtend { byte: addr, value } => { out.push(addr.as_u32()); out.push(value.as_u32()); }
        Mcopy { dst, src, len } | CallDataCopy { dst, offset: src, len }
        | ReturnDataCopy { dst, offset: src, len } | CodeCopy { dst, offset: src, len }
        | AddMod { lhs: dst, rhs: src, modulus: len } | MulMod { lhs: dst, rhs: src, modulus: len }
        | Create { value: dst, offset: src, len } => {
            out.push(dst.as_u32()); out.push(src.as_u32()); out.push(len.as_u32());
        }
        Create2 { value, offset, len, salt } => {
            out.push(value.as_u32()); out.push(offset.as_u32());
            out.push(len.as_u32()); out.push(salt.as_u32());
        }
        Keccak256 { offset, len } | Log0 { offset, len } => {
            out.push(offset.as_u32()); out.push(len.as_u32());
        }
        Log1 { offset, len, topic0 } => {
            out.push(offset.as_u32()); out.push(len.as_u32()); out.push(topic0.as_u32());
        }
        Log2 { offset, len, topic0, topic1 } => {
            out.push(offset.as_u32()); out.push(len.as_u32());
            out.push(topic0.as_u32()); out.push(topic1.as_u32());
        }
        Log3 { offset, len, topic0, topic1, topic2 } => {
            out.push(offset.as_u32()); out.push(len.as_u32());
            out.push(topic0.as_u32()); out.push(topic1.as_u32()); out.push(topic2.as_u32());
        }
        Log4 { offset, len, topic0, topic1, topic2, topic3 } => {
            out.push(offset.as_u32()); out.push(len.as_u32());
            out.push(topic0.as_u32()); out.push(topic1.as_u32());
            out.push(topic2.as_u32()); out.push(topic3.as_u32());
        }
        Call { gas, addr, value, args_offset, args_len, ret_offset, ret_len } => {
            for v in [gas, addr, value, args_offset, args_len, ret_offset, ret_len] {
                out.push(v.as_u32());
            }
        }
        StaticCall { gas, addr, args_offset, args_len, ret_offset, ret_len }
        | DelegateCall { gas, addr, args_offset, args_len, ret_offset, ret_len } => {
            for v in [gas, addr, args_offset, args_len, ret_offset, ret_len] {
                out.push(v.as_u32());
            }
        }
        MakeContractFieldRef { .. } | Msize | CallValue | ReturnDataSize | CallDataSize
        | CodeSize | Address | Caller | Origin | GasPrice | CoinBase | Timestamp | Number
        | PrevRandao | GasLimit | ChainId | BaseFee | SelfBalance | Gas
        | CurrentCodeRegionLen | CodeRegionOffset { .. } | CodeRegionLen { .. }
        | CallDataSelector => {}
    }
}

fn terminator_used_locals(term: &RTerminator<'_>) -> Vec<u32> {
    let mut locals = Vec::new();
    match term {
        RTerminator::Goto(_) | RTerminator::Trap | RTerminator::Stop => {}
        RTerminator::Branch { cond, .. } => locals.push(cond.as_u32()),
        RTerminator::Return(v) => { if let Some(v) = v { locals.push(v.as_u32()); } }
        RTerminator::SelfDestruct { beneficiary } => locals.push(beneficiary.as_u32()),
        RTerminator::ReturnData { offset, len } | RTerminator::Revert { offset, len } => {
            locals.push(offset.as_u32()); locals.push(len.as_u32());
        }
        RTerminator::TerminalCall { args, .. } => {
            for a in args.iter() { locals.push(a.as_u32()); }
        }
        RTerminator::SwitchScalar { discr, .. } => locals.push(discr.as_u32()),
        RTerminator::MatchEnumTag { tag, .. } => locals.push(tag.as_u32()),
    }
    locals
}
