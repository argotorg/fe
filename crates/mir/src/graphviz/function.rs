use std::fmt::Write;

use dot2::{label, Id};

use crate::{analysis::ControlFlowGraph, db::MirDb, ir::FunctionId, pretty_print::PrettyPrint};

use super::block::BlockNode;

#[derive(Debug, Clone, Copy)]
pub(super) struct FunctionNode {
    pub(super) func: FunctionId,
}

impl FunctionNode {
    pub(super) fn new(func: FunctionId) -> Self {
        Self { func }
    }

    pub(super) fn subgraph_id(self) -> Option<Id<'static>> {
        dot2::Id::new(format!("cluster_{}", self.func.0)).ok()
    }

    pub(super) fn label(self, db: &dyn MirDb) -> label::Text<'static> {
        let mut label = self.signature(db);
        write!(label, r#"<br/><br align="left"/>"#).unwrap();

        // Maps local value id to local name.
        let body = self.func.body(db);
        for local in body.store.locals() {
            local.pretty_print(db, &body.store, &mut label).unwrap();
            write!(
                label,
                r#" =&gt; {}<br align="left"/>"#,
                body.store.local_name(*local).unwrap()
            )
            .unwrap();
        }

        label::Text::HtmlStr(label.into())
    }

    pub(super) fn blocks(self, db: &dyn MirDb) -> Vec<BlockNode> {
        let body = self.func.body(db);
        // We use control flow graph to collect reachable blocks.
        let cfg = ControlFlowGraph::compute(&body);
        cfg.post_order()
            .map(|block| BlockNode::new(self.func, block))
            .collect()
    }

    fn signature(self, db: &dyn MirDb) -> String {
        let body = self.func.body(db);

        let sig_data = self.func.signature(db);
        let mut sig = format!("fn {}(", self.func.debug_name(db));

        let params = &sig_data.params;
        let param_len = params.len();
        for (i, param) in params.iter().enumerate() {
            let name = &param.name;
            let ty = param.ty;
            write!(&mut sig, "{name}: ").unwrap();
            ty.pretty_print(db, &body.store, &mut sig).unwrap();
            if param_len - 1 != i {
                write!(sig, ", ").unwrap();
            }
        }
        write!(sig, ")").unwrap();

        let ret_ty = self.func.return_type(db);
        if let Some(ret_ty) = ret_ty {
            write!(sig, " -> ").unwrap();
            ret_ty.pretty_print(db, &body.store, &mut sig).unwrap();
        }

        dot2::escape_html(&sig)
    }
}
