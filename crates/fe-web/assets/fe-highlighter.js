// fe-highlighter.js — Client-side tree-sitter syntax highlighting for Fe code.
//
// Provides window.FeHighlighter singleton:
//   init()                — async, loads WASM + compiles query
//   isReady()             — synchronous readiness check
//   highlightFe(src, scip) — returns highlighted HTML string
//
// WASM binaries and highlights.scm are injected as template placeholders
// by the Rust build (base64-encoded). No network fetches needed.

(function () {
  "use strict";

  var TS_WASM_B64 = "%%TS_WASM_B64%%";
  var FE_WASM_B64 = "%%FE_WASM_B64%%";
  var HIGHLIGHTS_SCM = "%%HIGHLIGHTS_SCM%%";

  // Capture names that should produce type links when ScipStore is available.
  var LINKABLE_CAPTURES = {
    "type": true,
    "type.builtin": true,
    "type.interface": true,
    "type.enum.variant": true,
    "function": true,
  };

  var parser = null;
  var query = null;
  var ready = false;

  function b64ToUint8(b64) {
    var bin = atob(b64);
    var arr = new Uint8Array(bin.length);
    for (var i = 0; i < bin.length; i++) arr[i] = bin.charCodeAt(i);
    return arr;
  }

  function escHtml(s) {
    return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }

  /** Look up a name in ScipStore. Returns {doc_url, symbol} or null. */
  function scipLookup(scip, text) {
    if (!scip || !scip.search) return null;
    try {
      var results = JSON.parse(scip.search(text));
      for (var i = 0; i < results.length; i++) {
        if (results[i].display_name === text && results[i].doc_url) {
          return results[i];
        }
      }
    } catch (_) {}
    return null;
  }

  async function init() {
    if (ready) return;
    var tsWasm = b64ToUint8(TS_WASM_B64);
    await TreeSitter.init({ wasmBinary: tsWasm });
    parser = new TreeSitter();
    var feWasm = b64ToUint8(FE_WASM_B64);
    var feLang = await TreeSitter.Language.load(feWasm);
    parser.setLanguage(feLang);
    query = feLang.query(HIGHLIGHTS_SCM);
    ready = true;
    document.dispatchEvent(new CustomEvent("fe-highlighter-ready"));
  }

  function isReady() {
    return ready;
  }

  /**
   * Pad a code fragment with stub syntax so tree-sitter can produce a proper
   * AST instead of ERROR nodes. The caller only uses captures within the
   * original source length, so the padding is invisible in the output.
   */
  function padForParse(source) {
    var s = source.trimEnd();
    // Signatures for types/traits/impls/fns lack a body — append one
    if (/\b(trait|struct|enum|contract|impl|fn)\b/.test(s) && s.indexOf("{") === -1) {
      return s + " {}";
    }
    return source;
  }

  /**
   * Parse and highlight Fe source code.
   *
   * @param {string} source — raw Fe code
   * @param {object|null} scip — ScipStore instance (for type linking)
   * @returns {string} HTML with <span class="hl-*"> and <a class="type-link">
   */
  function highlightFe(source, scip) {
    if (!ready) return escHtml(source);

    var parseSource = padForParse(source);
    var tree = parser.parse(parseSource);
    var captures = query.captures(tree.rootNode);
    tree.delete();

    // Sort captures by startIndex, then by length descending (outermost first).
    // For overlapping captures, innermost (shortest) wins — we process outermost
    // first but let innermost overwrite.
    captures.sort(function (a, b) {
      var d = a.node.startIndex - b.node.startIndex;
      if (d !== 0) return d;
      // Longer (outermost) first so inner captures overwrite
      return (b.node.endIndex - b.node.startIndex) - (a.node.endIndex - a.node.startIndex);
    });

    // Build an array of character-level capture assignments.
    // Each position gets the capture name of the innermost (last-written) capture.
    // Only covers original source length — padding captures are ignored.
    var len = source.length;
    var charCapture = new Array(len);
    for (var ci = 0; ci < captures.length; ci++) {
      var cap = captures[ci];
      var si = cap.node.startIndex;
      var ei = cap.node.endIndex;
      var name = cap.name;
      for (var k = si; k < ei && k < len; k++) {
        charCapture[k] = name;
      }
    }

    // Walk through source, grouping contiguous runs of the same capture.
    var html = "";
    var pos = 0;
    while (pos < len) {
      var capName = charCapture[pos];
      // Find the end of this run
      var runEnd = pos + 1;
      while (runEnd < len && charCapture[runEnd] === capName) runEnd++;
      var text = source.slice(pos, runEnd);

      if (!capName) {
        // No capture — plain text
        html += escHtml(text);
      } else {
        var cssClass = "hl-" + capName.replace(/\./g, "-");

        // Check if this capture should be type-linked
        if (LINKABLE_CAPTURES[capName] && scip) {
          var match = scipLookup(scip, text);
          if (match) {
            html += '<a href="#' + escHtml(match.doc_url) + '" class="' + cssClass + ' type-link">' + escHtml(text) + "</a>";
            pos = runEnd;
            continue;
          }
        }

        html += '<span class="' + cssClass + '">' + escHtml(text) + "</span>";
      }
      pos = runEnd;
    }

    return html;
  }

  window.FeHighlighter = {
    init: init,
    isReady: isReady,
    highlightFe: highlightFe,
  };

  // Auto-init on load
  init().catch(function (e) {
    console.error("[fe-highlighter] init failed:", e);
  });
})();
