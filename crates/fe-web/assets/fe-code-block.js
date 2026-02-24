// <fe-code-block> — Custom element for syntax-highlighted Fe code blocks.
//
// Content is always raw text. If FeHighlighter is available, it will be
// syntax-highlighted and type-linked client-side via tree-sitter WASM.
//
// Attributes:
//   lang         — language name (default "fe")
//   line-numbers — show line number gutter
//   collapsed    — start collapsed with <details>/<summary>
//   data-file    — SCIP source file path for positional symbol resolution
//   data-scope   — SCIP scope path for signature code blocks (set by server)

class FeCodeBlock extends HTMLElement {
  connectedCallback() {
    // Grab raw text before any rendering clears it
    this._rawSource = this.textContent;
    this.render();
  }

  render() {
    var lang = this.getAttribute("lang") || "fe";
    var showLineNumbers = this.hasAttribute("line-numbers");
    var collapsed = this.hasAttribute("collapsed");
    var source = this._rawSource || "";

    var wrapper = document.createElement("div");
    wrapper.className = "fe-code-block-wrapper";

    var pre = document.createElement("pre");
    pre.className = "fe-code-pre";

    var code = document.createElement("code");
    code.className = "language-" + lang;

    // Client-side highlighting via tree-sitter WASM (pure syntax coloring)
    if (lang === "fe" && window.FeHighlighter && window.FeHighlighter.isReady()) {
      code.innerHTML = window.FeHighlighter.highlightFe(source);
      this._highlighted = true;
    } else {
      code.textContent = source;
      this._highlighted = false;

      // If highlighter not ready yet, listen for it and re-render once
      if (lang === "fe" && !this._waitingForHighlighter) {
        this._waitingForHighlighter = true;
        var self = this;
        document.addEventListener("fe-highlighter-ready", function onReady() {
          document.removeEventListener("fe-highlighter-ready", onReady);
          self._waitingForHighlighter = false;
          // Re-render now that highlighter is available
          self.innerHTML = "";
          self.render();
        });
      }
    }

    // Clear original content before appending rendered version
    this.innerHTML = "";

    if (showLineNumbers) {
      var lines = code.innerHTML.split("\n");
      // Trim trailing empty line from trailing newline in source
      if (lines.length > 1 && lines[lines.length - 1] === "") {
        lines = lines.slice(0, -1);
      }
      var gutter = document.createElement("div");
      gutter.className = "fe-line-numbers";
      gutter.setAttribute("aria-hidden", "true");
      for (var i = 1; i <= lines.length; i++) {
        var span = document.createElement("span");
        span.textContent = i;
        gutter.appendChild(span);
      }
      wrapper.appendChild(gutter);
    }

    pre.appendChild(code);
    wrapper.appendChild(pre);

    if (collapsed) {
      var details = document.createElement("details");
      var summary = document.createElement("summary");
      summary.textContent = lang + " code";
      details.appendChild(summary);
      details.appendChild(wrapper);
      this.appendChild(details);
    } else {
      this.appendChild(wrapper);
    }

    // If SCIP is available, make highlighted spans interactive
    this._scipAnnotated = false;
    this._setupScipInteraction(code);

    // Walk highlighted spans and add type links via ScipStore name lookup
    // (fallback for code blocks without data-file or where positional resolution
    // didn't annotate anything)
    if (!this._scipAnnotated) {
      this._setupNameBasedLinking(code);
    }

    // Listen for live diagnostics from LSP
    this._setupLspDiagnostics(code);
  }

  /** Add click-to-navigate and hover highlighting on spans using ScipStore. */
  _setupScipInteraction(codeEl) {
    var scip = window.FE_SCIP;
    if (!scip) return;

    var file = this.getAttribute("data-file") || this.getAttribute("data-scope");
    if (!file) return;

    var self = this;

    // Path 1: Source file blocks with positional span attributes (data-line/data-col)
    var lineSpans = codeEl.querySelectorAll("span[data-line]");
    if (lineSpans.length > 0) {
      // Pre-assign role-aware CSS classes to all positional spans
      for (var i = 0; i < lineSpans.length; i++) {
        var span = lineSpans[i];
        var l = parseInt(span.getAttribute("data-line"), 10);
        var c = parseInt(span.getAttribute("data-col"), 10);
        var occ = scip.resolveOccurrence(file, l, c);
        if (occ) {
          var hash = scip.symbolHash(occ.sym);
          span.classList.add("sym-" + hash);
          if (occ.def) span.classList.add("sym-d-" + hash);
          else span.classList.add("sym-r-" + hash);
          span.setAttribute("data-sym", occ.sym);
        }
      }
    } else if (this._highlighted) {
      // Path 2: Signature blocks — resolve tree-sitter spans via character offset
      var source = this._rawSource || "";
      if (!source) return;

      // Build line-start index for offset→(line,col) conversion
      var lineStarts = [0];
      for (var si = 0; si < source.length; si++) {
        if (source.charCodeAt(si) === 10) lineStarts.push(si + 1);
      }

      function charToLineCol(pos) {
        var lo = 0, hi = lineStarts.length - 1;
        while (lo < hi) {
          var mid = (lo + hi + 1) >>> 1;
          if (lineStarts[mid] <= pos) lo = mid;
          else hi = mid - 1;
        }
        return [lo, pos - lineStarts[lo]];
      }

      // Walk DOM tree tracking character offset, resolve each span
      var offset = 0;
      var annotated = false;
      function walk(node) {
        var children = node.childNodes;
        for (var ci = 0; ci < children.length; ci++) {
          var child = children[ci];
          if (child.nodeType === 3) { // TEXT_NODE
            offset += child.textContent.length;
          } else if (child.nodeType === 1) { // ELEMENT_NODE
            var startOff = offset;
            if (child.tagName === "SPAN") {
              var lc = charToLineCol(startOff);
              var occ = scip.resolveOccurrence(file, lc[0], lc[1]);
              if (occ) {
                var hash = scip.symbolHash(occ.sym);
                child.classList.add("sym-" + hash);
                if (occ.def) child.classList.add("sym-d-" + hash);
                else child.classList.add("sym-r-" + hash);
                child.setAttribute("data-sym", occ.sym);
                annotated = true;
              }
            }
            walk(child);
          }
        }
      }
      walk(codeEl);

      if (annotated) self._scipAnnotated = true;
    }

    // Universal event handlers for any span with data-sym
    codeEl.addEventListener("click", function (e) {
      var target = e.target;
      if (target.tagName !== "SPAN" && target.tagName !== "A") return;
      var sym = target.getAttribute("data-sym");
      if (!sym) {
        // Fallback: try data-line/data-col for legacy spans
        var lineAttr = target.getAttribute("data-line");
        var colAttr = target.getAttribute("data-col");
        if (lineAttr && colAttr) {
          sym = scip.resolveSymbol(file, parseInt(lineAttr, 10), parseInt(colAttr, 10));
        }
      }
      if (sym) {
        var docPath = scip.docUrl(sym);
        if (docPath) location.hash = "#" + docPath;
      }
    });

    codeEl.addEventListener("mouseover", function (e) {
      var target = e.target;
      if (target.tagName !== "SPAN" && target.tagName !== "A") return;

      var sym = target.getAttribute("data-sym");
      if (!sym) {
        var lineAttr = target.getAttribute("data-line");
        var colAttr = target.getAttribute("data-col");
        if (lineAttr && colAttr) {
          sym = scip.resolveSymbol(file, parseInt(lineAttr, 10), parseInt(colAttr, 10));
        }
      }
      if (!sym) return;

      // Tooltip from SCIP metadata
      var info = scip.symbolInfo(sym);
      if (info) {
        try {
          var parsed = JSON.parse(info);
          target.title = parsed.display_name || sym;
        } catch (_) {}
      }

      target.style.cursor = scip.docUrl(sym) ? "pointer" : "default";
      feHighlight(scip.symbolHash(sym));
    });

    codeEl.addEventListener("mouseout", function (e) {
      if (e.target.tagName === "SPAN" || e.target.tagName === "A") {
        e.target.style.cursor = "";
        feUnhighlight();
      }
    });
  }

  /** CSS classes on highlighted spans that represent linkable names. */
  static LINKABLE_CLASSES = [
    "hl-type", "hl-type-builtin", "hl-type-interface", "hl-type-enum-variant", "hl-function"
  ];

  /**
   * Walk highlighted spans, look up type/function names in ScipStore,
   * and wrap matches in <a> links with hover highlighting.
   */
  _setupNameBasedLinking(codeEl) {
    var scip = window.FE_SCIP;
    if (!scip) return;

    var linkableSet = {};
    for (var i = 0; i < FeCodeBlock.LINKABLE_CLASSES.length; i++) {
      linkableSet[FeCodeBlock.LINKABLE_CLASSES[i]] = true;
    }

    var spans = codeEl.querySelectorAll("span");
    for (var si = 0; si < spans.length; si++) {
      var span = spans[si];
      // Check if this span has a linkable highlight class
      var isLinkable = false;
      for (var ci = 0; ci < span.classList.length; ci++) {
        if (linkableSet[span.classList[ci]]) { isLinkable = true; break; }
      }
      if (!isLinkable) continue;

      var text = span.textContent;
      // Strip generic params if present (e.g. "AbiDecoder<A" → "AbiDecoder")
      var ltIdx = text.indexOf("<");
      var lookupName = ltIdx > 0 ? text.slice(0, ltIdx) : text;
      if (!lookupName) continue;

      var match = this._scipLookupName(scip, lookupName);
      if (!match) continue;

      // Create an anchor wrapping the identifier text
      var a = document.createElement("a");
      a.href = "#" + match.doc_url;
      a.className = span.className + " type-link";

      var symClass = scip.symbolClass(match.symbol);
      a.classList.add(symClass);

      if (ltIdx > 0) {
        // Only link the identifier part, keep generic params in the span
        a.textContent = lookupName;
        // Replace span content: <a>Name</a><genericSuffix>
        span.textContent = text.slice(ltIdx);
        span.parentNode.insertBefore(a, span);
      } else {
        a.textContent = text;
        span.parentNode.replaceChild(a, span);
      }

      // Hover: highlight all same-symbol occurrences
      var symHash = scip.symbolHash(match.symbol);
      a.addEventListener("mouseenter", (function (h) {
        return function () { feHighlight(h); };
      })(symHash));
      a.addEventListener("mouseleave", feUnhighlight);

      // Tooltip from SCIP docs
      var info = scip.symbolInfo(match.symbol);
      if (info) {
        try {
          var parsed = JSON.parse(info);
          if (parsed.documentation && parsed.documentation.length > 0) {
            a.title = parsed.documentation[0].replace(/```[\s\S]*?```/g, "").trim();
          }
        } catch (_) {}
      }
    }
  }

  /** Look up a name in ScipStore. Returns {doc_url, symbol} or null. */
  _scipLookupName(scip, name) {
    try {
      var results = JSON.parse(scip.search(name));
      for (var i = 0; i < results.length; i++) {
        if (results[i].display_name === name && results[i].doc_url) {
          return results[i];
        }
      }
    } catch (_) {}
    return null;
  }

  /** Listen for LSP diagnostics and underline affected lines. */
  _setupLspDiagnostics(codeEl) {
    var file = this.getAttribute("data-file");
    if (!file) return;

    var self = this;
    document.addEventListener("fe-diagnostics", function (e) {
      var detail = e.detail;
      // Match by file path suffix (LSP uses full URIs)
      if (!detail.uri || !detail.uri.endsWith(file)) return;

      // Remove previous diagnostic markers
      var old = self.querySelectorAll(".fe-diagnostic-marker");
      for (var i = 0; i < old.length; i++) old[i].remove();

      // Add new markers
      var diags = detail.diagnostics || [];
      for (var j = 0; j < diags.length; j++) {
        var diag = diags[j];
        var line = diag.range && diag.range.start ? diag.range.start.line : -1;
        if (line < 0) continue;

        var marker = document.createElement("div");
        marker.className = "fe-diagnostic-marker";
        marker.setAttribute("data-severity", diag.severity || 1);
        marker.textContent = diag.message || "";
        marker.title = diag.message || "";
        marker.style.cssText = "color: var(--diag-color, #e55); font-size: 0.85em; padding-left: 2ch;";
        codeEl.parentNode.appendChild(marker);
      }
    });
  }
}

customElements.define("fe-code-block", FeCodeBlock);
