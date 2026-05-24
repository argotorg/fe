use std::collections::BTreeMap;

use shape_address::{
    DimensionDigests, ShapeCyclePolicy, ShapeDimension, ShapeGraph, ShapeGraphHashes,
    ShapeHashPolicy, ShapeNodeKey, ShapeViewMode, hash_shape_graph,
};

use crate::{
    ShapeBucketArgs, ShapeBytecodeArgs, ShapeCommand, ShapeDiffArgs, ShapeDimensionArg,
    ShapeEmitArgs, ShapeExplainArgs, ShapePolicyArgs, ShapeViewModeArg,
};

pub(crate) fn run_shape_command(command: &ShapeCommand) -> Result<String, String> {
    match command {
        ShapeCommand::Emit(args) => run_shape_emit(args),
        ShapeCommand::Explain(args) => run_shape_explain(args),
        ShapeCommand::Diff(args) => run_shape_diff(args),
        ShapeCommand::Bucket(args) => run_shape_bucket(args),
        ShapeCommand::LoopDiff(args) => run_loop_shape_diff(args),
        ShapeCommand::LoopBucket(args) => run_loop_shape_bucket(args),
    }
}

fn run_shape_emit(args: &ShapeEmitArgs) -> Result<String, String> {
    let view = build_view(&args.input)?;
    let mut out = render_shape_header("Shape emit", &view);
    out.push_str("\nGraph hashes:\n");
    push_digests(&mut out, &view.hashes.graph);
    out.push_str("\nNode hashes:\n");
    for (key, hashes) in &view.hashes.nodes {
        out.push_str(&format!("  {}\n", key.canonical_key()));
        out.push_str("    local: ");
        push_inline_digests(&mut out, &hashes.local);
        out.push('\n');
        out.push_str("    tree:  ");
        push_inline_digests(&mut out, &hashes.tree);
        out.push('\n');
    }
    Ok(out)
}

fn run_shape_explain(args: &ShapeExplainArgs) -> Result<String, String> {
    let view = build_view(&args.input)?;
    let mut out = render_shape_header("Shape explain", &view);
    out.push_str("\nMeaning:\n");
    out.push_str("  Shape hashes are derived content views keyed by OriginExportKey; they are not compiler identity.\n");
    out.push_str(
        "  structure: opcode sequence, node kind, ordered child labels, and graph edge labels.\n",
    );
    out.push_str("  constants: literal PUSH immediate bytes and other constant fields.\n");
    out.push_str("  names/types/trace-events: present for cross-phase policy compatibility; this bytecode adapter does not currently populate them.\n");
    if let Some(pc) = args.pc {
        let key = bytecode_pc_key(&args.input.owner, pc)?;
        out.push_str(&format!("\nPC {pc} node: {}\n", key.canonical_key()));
        match view.hashes.nodes.get(&key) {
            Some(hashes) => {
                out.push_str("  local: ");
                push_inline_digests(&mut out, &hashes.local);
                out.push('\n');
                out.push_str("  tree:  ");
                push_inline_digests(&mut out, &hashes.tree);
                out.push('\n');
            }
            None => out.push_str("  no shape node exists for this PC in the decoded bytecode\n"),
        }
    } else {
        out.push_str("\nGraph root:\n");
        push_digests(&mut out, &view.hashes.graph);
    }
    Ok(out)
}

fn run_shape_diff(args: &ShapeDiffArgs) -> Result<String, String> {
    render_diff("Shape diff", None, args)
}

fn run_loop_shape_diff(args: &ShapeDiffArgs) -> Result<String, String> {
    render_diff(
        "Loop shape diff",
        Some(
            "Scope: bytecode loop-region candidate supplied by the caller; this is a derived content view, not loop identity.",
        ),
        args,
    )
}

fn render_diff(title: &str, note: Option<&str>, args: &ShapeDiffArgs) -> Result<String, String> {
    let left = build_view(&ShapeBytecodeArgs {
        owner: args.owner.clone(),
        function: args.function.clone(),
        bytecode_hex: args.left_bytecode_hex.clone(),
        policy: args.policy.clone(),
    })?;
    let right = build_view(&ShapeBytecodeArgs {
        owner: args.owner.clone(),
        function: args.function.clone(),
        bytecode_hex: args.right_bytecode_hex.clone(),
        policy: args.policy.clone(),
    })?;
    let mut out = format!("{title}\n");
    out.push_str("Only hash equality is reported; this is not semantic equivalence.\n\n");
    if let Some(note) = note {
        out.push_str(note);
        out.push_str("\n\n");
    }
    for dimension in &left.policy.dimensions {
        let left_digest = left.hashes.graph.get(*dimension);
        let right_digest = right.hashes.graph.get(*dimension);
        let status = if left_digest == right_digest {
            "same"
        } else {
            "changed"
        };
        out.push_str(&format!(
            "  {:<12} {status:<7} left={} right={}\n",
            dimension.as_str(),
            short_digest(left_digest),
            short_digest(right_digest),
        ));
    }
    Ok(out)
}

fn run_shape_bucket(args: &ShapeBucketArgs) -> Result<String, String> {
    render_bucket("Shape bucket", None, args)
}

fn run_loop_shape_bucket(args: &ShapeBucketArgs) -> Result<String, String> {
    render_bucket(
        "Loop shape bucket",
        Some(
            "Scope: bytecode loop-region candidates supplied by the caller; buckets are derived content views, not compiler identity.",
        ),
        args,
    )
}

fn render_bucket(
    title: &str,
    note: Option<&str>,
    args: &ShapeBucketArgs,
) -> Result<String, String> {
    let dimension = shape_dimension(args.dimension);
    let mut buckets = BTreeMap::<String, Vec<String>>::new();
    let policy_args = ShapePolicyArgs {
        dimensions: vec![args.dimension],
        ..args.policy.clone()
    };
    for (index, hex) in args.bytecode_hex.iter().enumerate() {
        let view = build_view(&ShapeBytecodeArgs {
            owner: args.owner.clone(),
            function: args.function.clone(),
            bytecode_hex: hex.clone(),
            policy: policy_args.clone(),
        })?;
        let digest =
            view.hashes.graph.get(dimension).ok_or_else(|| {
                format!("shape policy did not produce {} digest", dimension.as_str())
            })?;
        buckets
            .entry(digest.as_str().to_string())
            .or_default()
            .push(format!("#{index}:0x{}", normalize_hex(hex)?));
    }

    let mut out = format!(
        "{title}\nDimension: {}\nVariants: {}\nBuckets: {}\n\n",
        dimension.as_str(),
        args.bytecode_hex.len(),
        buckets.len(),
    );
    if let Some(note) = note {
        out.push_str(note);
        out.push_str("\n\n");
    }
    for (digest, variants) in buckets {
        out.push_str(&format!("  {} [{}]\n", &digest[..16], variants.join(", ")));
    }
    Ok(out)
}

struct ShapeView {
    graph: ShapeGraph,
    policy: ShapeHashPolicy,
    hashes: ShapeGraphHashes,
    byte_len: usize,
}

fn build_view(args: &ShapeBytecodeArgs) -> Result<ShapeView, String> {
    let bytecode = parse_bytecode_hex(&args.bytecode_hex)?;
    let graph = codegen::shape::describe_bytecode_shape(&args.owner, &args.function, &bytecode)
        .map_err(|err| format!("failed to describe bytecode shape: {err}"))?;
    let policy = shape_policy(&args.policy)?;
    let hashes = hash_shape_graph(&policy, &graph)
        .map_err(|err| format!("failed to hash bytecode shape: {err}"))?;
    Ok(ShapeView {
        graph,
        policy,
        hashes,
        byte_len: bytecode.len(),
    })
}

fn shape_policy(args: &ShapePolicyArgs) -> Result<ShapeHashPolicy, String> {
    let dimensions = args.dimensions.iter().copied().map(shape_dimension);
    ShapeHashPolicy::with_dimensions(
        args.level.clone(),
        dimensions,
        shape_view_mode(args.view_mode),
        ShapeCyclePolicy::Reject,
    )
    .map_err(|err| format!("invalid shape policy: {err}"))
}

fn render_shape_header(title: &str, view: &ShapeView) -> String {
    format!(
        "{title}\nGraph: {}\nPolicy: {} ({}, {}, {})\nBytes: {}\nNodes: {}\nEdges: {}\n",
        view.graph.graph_key.canonical_key(),
        view.hashes.policy_id.display_short(),
        view.policy.level.as_str(),
        view.policy.view_mode.as_str(),
        view.policy.cycle_policy.as_str(),
        view.byte_len,
        view.graph.nodes.len(),
        view.graph.children.len() + view.graph.edges.len(),
    )
}

fn parse_bytecode_hex(hex: &str) -> Result<Vec<u8>, String> {
    hex::decode(normalize_hex(hex)?).map_err(|err| format!("invalid bytecode hex: {err}"))
}

fn normalize_hex(hex: &str) -> Result<String, String> {
    let hex = hex.trim().strip_prefix("0x").unwrap_or(hex.trim());
    if hex.is_empty() {
        return Ok(String::new());
    }
    if hex.len() % 2 != 0 {
        return Err("bytecode hex must contain an even number of digits".to_string());
    }
    if !hex.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        return Err("bytecode hex contains non-hex characters".to_string());
    }
    Ok(hex.to_ascii_lowercase())
}

fn bytecode_pc_key(owner: &str, pc: u32) -> Result<ShapeNodeKey, String> {
    Ok(ShapeNodeKey::entity(
        common::origin::OriginExportKey::try_from_raw_parts(
            "bytecode.pc",
            owner,
            format!("pc:{pc}"),
        )
        .map_err(|err| format!("invalid bytecode PC origin key: {err}"))?,
    ))
}

fn shape_view_mode(arg: ShapeViewModeArg) -> ShapeViewMode {
    match arg {
        ShapeViewModeArg::IdentityBound => ShapeViewMode::IdentityBound,
        ShapeViewModeArg::AnonymousShape => ShapeViewMode::AnonymousShape,
    }
}

fn shape_dimension(arg: ShapeDimensionArg) -> ShapeDimension {
    match arg {
        ShapeDimensionArg::Structure => ShapeDimension::Structure,
        ShapeDimensionArg::Names => ShapeDimension::Names,
        ShapeDimensionArg::Constants => ShapeDimension::Constants,
        ShapeDimensionArg::Types => ShapeDimension::Types,
        ShapeDimensionArg::TraceEvents => ShapeDimension::TraceEvents,
    }
}

fn push_digests(out: &mut String, digests: &DimensionDigests) {
    for (dimension, digest) in digests.iter() {
        out.push_str(&format!(
            "  {:<12} {}\n",
            dimension.as_str(),
            digest.as_str()
        ));
    }
}

fn push_inline_digests(out: &mut String, digests: &DimensionDigests) {
    let mut first = true;
    for (dimension, digest) in digests.iter() {
        if !first {
            out.push_str(", ");
        }
        first = false;
        out.push_str(&format!(
            "{}={}",
            dimension.as_str(),
            digest.display_short()
        ));
    }
}

fn short_digest(digest: Option<&shape_address::ShapeDigest>) -> String {
    digest
        .map(|digest| digest.display_short().to_string())
        .unwrap_or_else(|| "missing".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn policy() -> ShapePolicyArgs {
        ShapePolicyArgs {
            level: "bytecode".to_string(),
            view_mode: ShapeViewModeArg::AnonymousShape,
            dimensions: vec![ShapeDimensionArg::Structure, ShapeDimensionArg::Constants],
        }
    }

    #[test]
    fn diff_reports_literal_only_change() {
        let output = run_shape_diff(&ShapeDiffArgs {
            owner: "contract:demo".to_string(),
            function: "function:runtime".to_string(),
            left_bytecode_hex: "6001".to_string(),
            right_bytecode_hex: "6002".to_string(),
            policy: policy(),
        })
        .unwrap();

        assert!(output.contains("structure    same"));
        assert!(output.contains("constants    changed"));
    }

    #[test]
    fn bucket_groups_variants_by_structure() {
        let output = run_shape_bucket(&ShapeBucketArgs {
            owner: "contract:demo".to_string(),
            function: "function:runtime".to_string(),
            bytecode_hex: vec!["6001".to_string(), "6002".to_string(), "01".to_string()],
            dimension: ShapeDimensionArg::Structure,
            policy: policy(),
        })
        .unwrap();

        assert!(output.contains("Variants: 3"));
        assert!(output.contains("Buckets: 2"));
    }

    #[test]
    fn loop_shape_commands_are_labeled_as_candidate_views() {
        let diff = run_loop_shape_diff(&ShapeDiffArgs {
            owner: "contract:demo".to_string(),
            function: "function:runtime".to_string(),
            left_bytecode_hex: "6001".to_string(),
            right_bytecode_hex: "6002".to_string(),
            policy: policy(),
        })
        .unwrap();
        assert!(diff.contains("Loop shape diff"));
        assert!(diff.contains("loop-region candidate"));

        let bucket = run_loop_shape_bucket(&ShapeBucketArgs {
            owner: "contract:demo".to_string(),
            function: "function:runtime".to_string(),
            bytecode_hex: vec!["6001".to_string(), "6002".to_string()],
            dimension: ShapeDimensionArg::Structure,
            policy: policy(),
        })
        .unwrap();
        assert!(bucket.contains("Loop shape bucket"));
        assert!(bucket.contains("derived content views"));
    }

    #[test]
    fn explain_reports_specific_pc_hashes() {
        let output = run_shape_explain(&ShapeExplainArgs {
            input: ShapeBytecodeArgs {
                owner: "contract:demo".to_string(),
                function: "function:runtime".to_string(),
                bytecode_hex: "6001".to_string(),
                policy: policy(),
            },
            pc: Some(0),
        })
        .unwrap();

        assert!(output.contains("PC 0 node"));
        assert!(output.contains("tree:"));
    }
}
