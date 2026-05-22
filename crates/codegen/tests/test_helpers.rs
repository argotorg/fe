#![allow(dead_code, unused_imports)]

use common::InputDb;
use common::hash_consumer::{DimHashes, HashConsumer};
use common::ir_describe::{DescribeCtx, IrDescribe};
use driver::DriverDataBase;
use fe_codegen::analyze::SourceAnalysis;

pub fn analyze(source: &str) -> SourceAnalysis {
    SourceAnalysis::from_source(source).expect("compile")
}

pub fn compile_mir_hashes(source: &str) -> (SourceAnalysis, Vec<(String, DimHashes)>) {
    let a = analyze(source);
    let hashes = a
        .functions
        .iter()
        .map(|f| (f.symbol.clone(), f.hashes.clone()))
        .collect();
    (a, hashes)
}

pub fn find_hash<'a>(hashes: &'a [(String, DimHashes)], substr: &str) -> Option<&'a DimHashes> {
    hashes
        .iter()
        .find(|(n, _)| n.contains(substr))
        .map(|(_, h)| h)
}

pub const BASE_CONTRACT: &str = r#"
msg Msg {
    #[selector = 0x11111111]
    Transfer { to: Address, amount: u256 } -> bool,
    #[selector = 0x22222222]
    Balance { account: Address } -> u256,
}

struct Store {
    balances: StorageMap<Address, u256>,
}

pub contract Token uses (ctx: Ctx) {
    mut store: Store

    recv Msg {
        Transfer { to, amount } -> bool uses (mut store, ctx) {
            let sender = ctx.caller()
            let bal = store.balances.get(key: sender)
            if bal < amount {
                return false
            }
            store.balances.set(key: sender, value: bal - amount)
            store.balances.set(key: to, value: store.balances.get(key: to) + amount)
            return true
        }

        Balance { account } -> u256 uses store {
            store.balances.get(key: account)
        }
    }
}
"#;

/// Extract function name and body pairs from Sonatina IR text.
pub fn extract_sonatina_functions(ir: &str) -> Vec<(String, String)> {
    let mut result = Vec::new();
    let mut current_name = None;
    let mut current_body = String::new();
    let mut brace_depth = 0i32;

    for line in ir.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("func ") {
            // Extract function name: func <vis> %name(...)
            if let Some(pct) = trimmed.find('%') {
                let after_pct = &trimmed[pct + 1..];
                let name_end = after_pct.find('(').unwrap_or(after_pct.len());
                current_name = Some(after_pct[..name_end].to_string());
            }
            current_body.clear();
            brace_depth = 0;
        }

        if trimmed.contains('{') {
            brace_depth += trimmed.matches('{').count() as i32;
        }
        if brace_depth > 0 {
            current_body.push_str(line);
            current_body.push('\n');
        }
        if trimmed.contains('}') {
            brace_depth -= trimmed.matches('}').count() as i32;
            if brace_depth <= 0 {
                if let Some(name) = current_name.take() {
                    result.push((name, current_body.clone()));
                }
                current_body.clear();
            }
        }
    }
    result
}

/// Normalize a Sonatina IR function body by replacing all block labels and
/// value names with canonical placeholders, so two structurally identical
/// functions compare equal regardless of name allocation.
pub fn normalize_sonatina_body(body: &str) -> String {
    let mut result = String::new();
    let mut val_map: std::collections::HashMap<String, String> = Default::default();
    let mut val_counter = 0u32;
    let mut block_map: std::collections::HashMap<String, String> = Default::default();
    let mut block_counter = 0u32;

    for line in body.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        // Skip the func declaration line — it contains the unique function name
        if trimmed.starts_with("func ") {
            continue;
        }

        let mut normalized = trimmed.to_string();

        // Replace block labels (block_N:)
        if let Some(colon_pos) = trimmed.find(':') {
            let label = &trimmed[..colon_pos];
            if label.starts_with("block_") || label.starts_with("bb") {
                let canonical = block_map
                    .entry(label.to_string())
                    .or_insert_with(|| {
                        let b = format!("BB{block_counter}");
                        block_counter += 1;
                        b
                    })
                    .clone();
                normalized = format!("{canonical}:{}", &trimmed[colon_pos + 1..]);
            }
        }

        // Replace value references (v0, v1, ...) with canonical names
        let mut out = String::new();
        let mut chars = normalized.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == 'v' && chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                let mut num = String::new();
                while chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                    num.push(chars.next().unwrap());
                }
                let key = format!("v{num}");
                let canonical = val_map
                    .entry(key)
                    .or_insert_with(|| {
                        let v = format!("V{val_counter}");
                        val_counter += 1;
                        v
                    })
                    .clone();
                out.push_str(&canonical);
            } else if ch == 'b' && matches!(chars.peek(), Some(&'l') | Some(&'b')) {
                // Potential block reference in jumps
                let mut label = String::from(ch);
                while chars
                    .peek()
                    .is_some_and(|c| c.is_alphanumeric() || *c == '_')
                {
                    label.push(chars.next().unwrap());
                }
                if label.starts_with("block_") || label.starts_with("bb") {
                    let canonical = block_map
                        .entry(label)
                        .or_insert_with(|| {
                            let b = format!("BB{block_counter}");
                            block_counter += 1;
                            b
                        })
                        .clone();
                    out.push_str(&canonical);
                } else {
                    out.push_str(&label);
                }
            } else {
                out.push(ch);
            }
        }

        result.push_str(&out);
        result.push('\n');
    }
    result
}

pub struct SimpleLeaf {
    pub tag: u64,
}

impl IrDescribe for SimpleLeaf {
    fn describe<C: common::ir_describe::IrConsumer>(&self, _cx: &DescribeCtx<'_>, c: &mut C) {
        c.enter_node("Leaf");
        c.field_u64(common::ir_describe::Dim::Structure, self.tag);
        c.exit_node();
    }
}

#[derive(Default)]
pub struct FieldCounter {
    pub nodes: u64,
    pub fields: u64,
}

impl common::ir_describe::IrConsumer for FieldCounter {
    fn enter_node(&mut self, _kind: &str) {
        self.nodes += 1;
    }
    fn exit_node(&mut self) {}
    fn field_u64(&mut self, _dim: common::ir_describe::Dim, _value: u64) {
        self.fields += 1;
    }
    fn field_bytes(&mut self, _dim: common::ir_describe::Dim, _value: &[u8]) {
        self.fields += 1;
    }
    fn field_str(&mut self, _dim: common::ir_describe::Dim, _value: &str) {
        self.fields += 1;
    }
    fn field_bool(&mut self, _dim: common::ir_describe::Dim, _value: bool) {
        self.fields += 1;
    }
    fn child<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, child: &T) {
        child.describe(cx, self);
    }
    fn children_ordered<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, children: &[T]) {
        for child in children {
            child.describe(cx, self);
        }
    }
    fn children_unordered<T: IrDescribe>(&mut self, cx: &DescribeCtx<'_>, children: &[T]) {
        self.children_ordered(cx, children);
    }
    fn graph_edge(&mut self, _label: &str, _target_id: u64) {
        self.fields += 1;
    }
    fn origin(&mut self, _origin: &common::provenance::ProvenanceNodeId) {}
    fn source_span(&mut self, _file: &str, _line: u32, _col: u32, _end_line: u32, _end_col: u32) {}
}
