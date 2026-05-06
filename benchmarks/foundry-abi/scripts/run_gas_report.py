#!/usr/bin/env python3
from __future__ import annotations

import csv
import re
import statistics
import subprocess
import sys
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
REPORT_DIR = ROOT / "reports"
RAW_REPORT = REPORT_DIR / "gas-report.txt"
CSV_REPORT = REPORT_DIR / "gas-deltas.csv"
MD_REPORT = REPORT_DIR / "gas-summary.md"

COMMAND = ["forge", "test", "--root", str(ROOT), "--offline", "--gas-report"]


def run_gas_report() -> str:
    proc = subprocess.run(COMMAND, capture_output=True, text=True)
    output = proc.stdout
    if proc.stderr:
        output = f"{output}\n{proc.stderr}" if output else proc.stderr
    print(output, end="")
    if proc.returncode != 0:
        raise SystemExit(proc.returncode)
    return output


def parse_contract_tables(text: str) -> dict[str, dict[str, int]]:
    contract_tables: dict[str, dict[str, int]] = defaultdict(dict)
    current_contract: str | None = None
    in_function_table = False

    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line.startswith("|"):
            continue

        cols = [part.strip() for part in line.strip("|").split("|")]
        if cols and cols[0].endswith("Contract"):
            current_contract = cols[0]
            in_function_table = False
            continue

        if cols and cols[0] == "Function Name":
            in_function_table = True
            continue

        if not in_function_table or current_contract is None:
            continue

        if len(cols) != 6:
            continue

        name = cols[0]
        if not name or name == "Deployment Cost" or name.startswith("-"):
            continue

        try:
            avg = int(cols[2])
        except ValueError:
            continue

        contract_tables[current_contract][name] = avg

    return contract_tables


def function_width(name: str) -> int:
    match = re.search(r"(Uint|Int)(\d+)$", name)
    if match:
        return int(match.group(2))
    if name.endswith("Uint"):
        return 256
    if name.endswith("Int256"):
        return 256
    return 0


def classify_bench_function(name: str) -> str:
    if "Matrix2x2" in name:
        return "nested-fixed-array"
    if "Array" in name:
        if "Pair" in name or "Triple" in name:
            if "String" in name:
                return "tuple-fixed-array-dynamic"
            return "tuple-fixed-array"
        if "String" in name:
            return "dynamic-fixed-array"
        return "fixed-array"
    if "Pair" in name or "Triple" in name:
        if name == "benchEchoPair" or "String" in name:
            return "tuple-dynamic"
        return "tuple-static"
    if name == "benchEchoString":
        return "dynamic-string"
    if name == "benchEchoBool":
        return "scalar-bool"
    if name == "benchEchoAddress":
        return "scalar-address"
    if name.startswith("benchEchoUint"):
        width = function_width(name)
        if width in {8, 16, 32, 64, 128, 256}:
            return "native-unsigned"
        return "custom-unsigned"
    if name.startswith("benchEchoInt"):
        width = function_width(name)
        if width in {8, 16, 32, 64, 128, 256}:
            return "native-signed"
        return "custom-signed"
    return "other"


def suite_summary(text: str) -> str:
    match = re.search(
        r"Ran\s+(\d+)\s+test suites?.*?:\s+(\d+)\s+tests passed,\s+(\d+)\s+failed,\s+(\d+)\s+skipped",
        text,
        re.DOTALL,
    )
    if not match:
        return "Suite summary not parsed."
    suites, passed, failed, skipped = match.groups()
    return f"{suites} suites, {passed} passed, {failed} failed, {skipped} skipped"


def write_reports(text: str) -> None:
    REPORT_DIR.mkdir(parents=True, exist_ok=True)
    RAW_REPORT.write_text(text, encoding="utf-8")

    tables = parse_contract_tables(text)
    sol_rows = tables.get("src/AbiRoundtripSol.sol:SolBenchCaller Contract", {})
    fe_rows = tables.get("src/AbiRoundtripSol.sol:FeBenchCaller Contract", {})

    records: list[dict[str, object]] = []
    for fn_name in sorted(set(sol_rows) & set(fe_rows)):
        sol_avg = sol_rows[fn_name]
        fe_avg = fe_rows[fn_name]
        delta = fe_avg - sol_avg
        pct = (delta / sol_avg * 100.0) if sol_avg else 0.0
        records.append(
            {
                "function": fn_name,
                "category": classify_bench_function(fn_name),
                "sol_avg": sol_avg,
                "fe_avg": fe_avg,
                "delta": delta,
                "delta_pct": pct,
            }
        )

    with CSV_REPORT.open("w", newline="", encoding="utf-8") as csv_file:
        writer = csv.DictWriter(
            csv_file,
            fieldnames=["function", "category", "sol_avg", "fe_avg", "delta", "delta_pct"],
        )
        writer.writeheader()
        writer.writerows(records)

    deltas = [int(row["delta"]) for row in records]
    avg_delta = statistics.mean(deltas) if deltas else 0.0
    median_delta = statistics.median(deltas) if deltas else 0.0

    category_rows: dict[str, list[dict[str, object]]] = defaultdict(list)
    for row in records:
        category_rows[str(row["category"])].append(row)

    regressions = sorted(records, key=lambda row: int(row["delta"]), reverse=True)[:15]
    improvements = sorted(records, key=lambda row: int(row["delta"]))[:15]

    lines: list[str] = [
        "# ABI Gas Summary",
        "",
        f"Generated: {datetime.now(timezone.utc).isoformat()}",
        "",
        f"Suite summary: {suite_summary(text)}",
        "",
        f"Raw report: `{RAW_REPORT.relative_to(ROOT)}`",
        "",
        f"CSV: `{CSV_REPORT.relative_to(ROOT)}`",
        "",
        "## Overall",
        "",
        f"- Bench functions compared: {len(records)}",
        f"- Mean Fe minus Solidity delta: {avg_delta:.2f} gas",
        f"- Median Fe minus Solidity delta: {median_delta:.2f} gas",
    ]

    if records:
        best = min(records, key=lambda row: int(row["delta"]))
        worst = max(records, key=lambda row: int(row["delta"]))
        lines.append(
            f"- Best delta: `{best['function']}` = {best['delta']} gas ({best['delta_pct']:.2f}%)"
        )
        lines.append(
            f"- Worst delta: `{worst['function']}` = {worst['delta']} gas ({worst['delta_pct']:.2f}%)"
        )

    category_stats: list[tuple[str, int, float, float, int, int]] = []
    for category, rows in category_rows.items():
        category_deltas = [int(row["delta"]) for row in rows]
        category_stats.append(
            (
                category,
                len(rows),
                statistics.mean(category_deltas),
                statistics.median(category_deltas),
                min(category_deltas),
                max(category_deltas),
            )
        )
    category_stats.sort(key=lambda item: item[2], reverse=True)

    lines.extend(
        [
            "",
            "## By Category",
            "",
            "| Category | Functions | Mean Delta | Median Delta | Best | Worst |",
            "| --- | ---: | ---: | ---: | ---: | ---: |",
        ]
    )
    for category, count, mean_delta, median_delta, best_delta, worst_delta in category_stats:
        lines.append(
            f"| `{category}` | {count} | {mean_delta:.2f} | {median_delta:.2f} | {best_delta} | {worst_delta} |"
        )

    if category_stats:
        hottest = category_stats[0]
        coolest = min(category_stats, key=lambda item: item[2])
        lines.extend(
            [
                "",
                "## Notable Patterns",
                "",
                f"- Highest mean regression category: `{hottest[0]}` at {hottest[2]:.2f} gas across {hottest[1]} functions.",
                f"- Lowest mean regression category: `{coolest[0]}` at {coolest[2]:.2f} gas across {coolest[1]} functions.",
            ]
        )

    def append_table(title: str, rows: list[dict[str, object]]) -> None:
        lines.extend(
            [
                "",
                f"## {title}",
                "",
                "| Function | Solidity Avg | Fe Avg | Delta | Delta % |",
                "| --- | ---: | ---: | ---: | ---: |",
            ]
        )
        for row in rows:
            lines.append(
                f"| `{row['function']}` | {row['sol_avg']} | {row['fe_avg']} | {row['delta']} | {row['delta_pct']:.2f}% |"
            )

    append_table("Largest Regressions", regressions)
    append_table("Largest Improvements", improvements)

    MD_REPORT.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> None:
    text = run_gas_report()
    write_reports(text)
    print(f"\nWrote {RAW_REPORT}")
    print(f"Wrote {CSV_REPORT}")
    print(f"Wrote {MD_REPORT}")


if __name__ == "__main__":
    main()
