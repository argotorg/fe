// fe-highlighter.js — Client-side tree-sitter syntax highlighting for Fe code.
//
// Provides window.FeHighlighter singleton:
//   init()              — async, loads WASM + compiles query
//   isReady()           — synchronous readiness check
//   highlightFe(source) — returns highlighted HTML string (pure syntax coloring)
//
// WASM binaries and highlights.scm are injected as template placeholders
// by the Rust build (base64-encoded). No network fetches needed.
//
// Type linking and hover interactivity are handled separately by
// fe-code-block.js using ScipStore — the highlighter only does coloring.

(function () {
  "use strict";

  var TS_WASM_B64 = "%%TS_WASM_B64%%";
  var FE_WASM_B64 = "%%FE_WASM_B64%%";
  var HIGHLIGHTS_SCM = "%%HIGHLIGHTS_SCM%%";

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
   *
   * Returns { source: paddedString, offset: charsAddedBefore }.
   */
  function padForParse(source) {
    var s = source.trimEnd();
    if (s.indexOf("{") !== -1) return { source: source, offset: 0 };

    // fn signatures containing Self need an impl wrapper so tree-sitter
    // recognizes Self as self_type rather than a plain identifier.
    if (/\bfn\b/.test(s) && /\bSelf\b/.test(s)) {
      var prefix = "impl X { ";
      return { source: prefix + s + " {} }", offset: prefix.length };
    }

    // Other signatures (trait, struct, enum, impl, fn) just need a body
    if (/\b(trait|struct|enum|contract|impl|fn)\b/.test(s)) {
      return { source: s + " {}", offset: 0 };
    }

    return { source: source, offset: 0 };
  }

  /**
   * Parse and highlight Fe source code (pure syntax coloring).
   *
   * @param {string} source — raw Fe code
   * @returns {string} HTML with <span class="hl-*"> elements
   */
  function highlightFe(source) {
    if (!ready) return escHtml(source);

    var padded = padForParse(source);
    var tree = parser.parse(padded.source);
    var captures = query.captures(tree.rootNode);
    tree.delete();

    var offset = padded.offset;

    // Sort captures by startIndex, then by length descending (outermost first).
    // For overlapping captures, innermost (shortest) wins — we process outermost
    // first but let innermost overwrite.
    captures.sort(function (a, b) {
      var d = a.node.startIndex - b.node.startIndex;
      if (d !== 0) return d;
      return (b.node.endIndex - b.node.startIndex) - (a.node.endIndex - a.node.startIndex);
    });

    // Build an array of character-level capture assignments.
    // Only covers original source length — padding captures are ignored.
    // Captures are shifted by -offset to account for any prefix padding.
    var len = source.length;
    var charCapture = new Array(len);
    for (var ci = 0; ci < captures.length; ci++) {
      var cap = captures[ci];
      var si = cap.node.startIndex - offset;
      var ei = cap.node.endIndex - offset;
      var name = cap.name;
      for (var k = Math.max(0, si); k < ei && k < len; k++) {
        charCapture[k] = name;
      }
    }

    // Walk through source, grouping contiguous runs of the same capture.
    var html = "";
    var pos = 0;
    while (pos < len) {
      var capName = charCapture[pos];
      var runEnd = pos + 1;
      while (runEnd < len && charCapture[runEnd] === capName) runEnd++;
      var text = source.slice(pos, runEnd);

      if (!capName) {
        html += escHtml(text);
      } else {
        var cssClass = "hl-" + capName.replace(/\./g, "-");
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
