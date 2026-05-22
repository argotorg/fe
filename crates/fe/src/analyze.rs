use std::fmt::Write;
use camino::Utf8PathBuf;
use codegen::analyze::SourceAnalysis;

pub fn analyze_command(path: &Utf8PathBuf, gas_focus: bool) {
    let source = match std::fs::read_to_string(path.as_str()) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read {path}: {e}");
            std::process::exit(1);
        }
    };

    let analysis = match SourceAnalysis::from_source(&source) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("error: compilation failed: {e}");
            std::process::exit(1);
        }
    };

    let mut out = String::new();
    write_report(&mut out, &analysis, path.as_str(), gas_focus);
    print!("{out}");
}

fn write_report(out: &mut String, a: &SourceAnalysis, path: &str, gas_focus: bool) {
    let ov = a.overview();

    writeln!(out, "Fe Compilation Analysis: {path}").unwrap();
    writeln!(out, "{}", "=".repeat(40 + path.len())).unwrap();
    writeln!(out).unwrap();

    writeln!(out, "Functions:  {} ({} unique structures, {:.1}% monomorphization)",
        ov.total_functions, ov.unique_structures, ov.dup_pct).unwrap();
    writeln!(out, "MIR stmts:  {}", ov.total_stmts).unwrap();
    writeln!(out, "Origin coverage: {:.1}%", ov.origin_coverage_pct).unwrap();
    writeln!(out).unwrap();

    // Category breakdown
    let cats = a.category_breakdown();
    writeln!(out, "Bytecode Breakdown").unwrap();
    writeln!(out, "{:<14} {:>6} {:>8} {:>7}", "Category", "Funcs", "Stmts", "%").unwrap();
    writeln!(out, "{}", "-".repeat(37)).unwrap();
    for c in &cats {
        writeln!(out, "{:<14} {:>6} {:>8} {:>5.1}%", c.name, c.count, c.stmts, c.pct).unwrap();
    }
    writeln!(out).unwrap();

    // Dedup
    let dedup = a.dedup_report();
    if !dedup.entries.is_empty() {
        let limit = if gas_focus { 10 } else { 5 };
        writeln!(out, "Deduplication Candidates (top {limit})").unwrap();
        writeln!(out, "{:<32} {:>6} {:>8} {:>8}", "Function", "Copies", "Stmts", "Wasted").unwrap();
        writeln!(out, "{}", "-".repeat(58)).unwrap();
        for e in dedup.entries.iter().take(limit) {
            let name = if e.representative.len() > 30 {
                format!("{}...", &e.representative[..27])
            } else {
                e.representative.clone()
            };
            writeln!(out, "{:<32} {:>6} {:>8} {:>8}", name, e.copies, e.stmts_per_copy, e.wasted).unwrap();
        }
        writeln!(out, "\nTotal wasted: {} stmts ({:.1}% of bytecode)", dedup.total_wasted, dedup.pct_wasted).unwrap();
        writeln!(out).unwrap();
    }

    // Effects
    let effects = a.effect_summary();
    if !effects.is_empty() {
        writeln!(out, "Effects").unwrap();
        writeln!(out, "{:<22} {:>6}", "Effect", "Count").unwrap();
        writeln!(out, "{}", "-".repeat(30)).unwrap();
        for e in &effects {
            writeln!(out, "{:<22} {:>6}", e.effect, e.count).unwrap();
        }
        writeln!(out).unwrap();
    }

    if gas_focus {
        let abi_stmts = cats.iter().find(|c| c.name == "ABI").map(|c| c.stmts).unwrap_or(0);
        let biz_stmts = cats.iter().find(|c| c.name == "Business").map(|c| c.stmts).unwrap_or(0);
        if abi_stmts > biz_stmts {
            writeln!(out, "Note: ABI encoding ({abi_stmts} stmts) exceeds business logic ({biz_stmts} stmts).").unwrap();
            writeln!(out, "Deduplication of ABI helpers is the highest-impact gas optimization.").unwrap();
        }
    }
}
