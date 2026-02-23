/**
 * remark-fe-highlight — Remark plugin that highlights Fe code blocks
 * using the `fe highlight` CLI with batch mode.
 *
 * Usage in astro.config.mjs:
 *   import remarkFeHighlight from '../fe/crates/fe-web/astro/remark-fe-highlight.mjs';
 *   // Add to remarkPlugins array (after remark-hide-directive if used):
 *   remarkPlugins: [remarkHideDirective, remarkFeHighlight]
 *
 * Options:
 *   feBinary — path to the `fe` binary (default: "fe")
 */

import { execFileSync } from "node:child_process";
import { visit } from "unist-util-visit";

export default function remarkFeHighlight(options = {}) {
  const feBinary = options.feBinary || "fe";

  return function transformer(tree) {
    // Collect all Fe code blocks
    const feBlocks = [];

    visit(tree, "code", (node, index, parent) => {
      if (node.lang && (node.lang === "fe" || node.lang.startsWith("fe,"))) {
        feBlocks.push({ node, index, parent, id: String(feBlocks.length) });
      }
    });

    if (feBlocks.length === 0) return;

    // Build batch input: one JSON line per code block
    const input = feBlocks
      .map((b) => JSON.stringify({ id: b.id, code: b.node.value }))
      .join("\n");

    let output;
    try {
      output = execFileSync(feBinary, ["highlight", "--batch", "--component"], {
        input,
        encoding: "utf-8",
        timeout: 30_000,
        maxBuffer: 10 * 1024 * 1024,
      });
    } catch (err) {
      // Graceful fallback: if `fe` is not found or fails, leave code blocks as-is
      const msg = err.code === "ENOENT"
        ? `fe binary not found at "${feBinary}". Fe code blocks will not be highlighted.`
        : `fe highlight failed: ${err.message}`;
      console.warn(`[remark-fe-highlight] ${msg}`);
      return;
    }

    // Parse output lines into a map of id → html
    const results = new Map();
    for (const line of output.trim().split("\n")) {
      if (!line) continue;
      try {
        const parsed = JSON.parse(line);
        results.set(parsed.id, parsed.html);
      } catch {
        // skip malformed lines
      }
    }

    // Replace code nodes with raw HTML
    for (const block of feBlocks) {
      const html = results.get(block.id);
      if (html) {
        block.parent.children[block.index] = {
          type: "html",
          value: html,
        };
      }
    }
  };
}
