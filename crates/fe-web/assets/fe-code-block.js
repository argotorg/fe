// <fe-code-block> — Custom element for syntax-highlighted Fe code blocks.
//
// Content is always raw text. If FeHighlighter is available, it will be
// syntax-highlighted and type-linked client-side via tree-sitter WASM.
//
// Attributes:
//   lang         — language name (default "fe")
//   line-numbers — show line number gutter
//   collapsed    — start collapsed with <details>/<summary>

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

    // Client-side highlighting via tree-sitter WASM
    if (lang === "fe" && window.FeHighlighter && window.FeHighlighter.isReady()) {
      code.innerHTML = window.FeHighlighter.highlightFe(source, window.FE_SCIP || null);
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
    this._setupScipInteraction(code);

    // Enrich type links with SCIP symbol data for hover highlighting
    this._setupTypeLinkHighlighting(code);

    // Listen for live diagnostics from LSP
    this._setupLspDiagnostics(code);
  }

  /** Add click-to-navigate on highlighted spans when ScipStore is loaded. */
  _setupScipInteraction(codeEl) {
    var scip = window.FE_SCIP;
    if (!scip) return;

    var file = this.getAttribute("data-file");
    if (!file) return;

    // Pre-assign symbol CSS classes to all positional spans
    var spans = codeEl.querySelectorAll("span[data-line]");
    for (var i = 0; i < spans.length; i++) {
      var span = spans[i];
      var l = parseInt(span.getAttribute("data-line"), 10);
      var c = parseInt(span.getAttribute("data-col"), 10);
      var sym = scip.resolveSymbol(file, l, c);
      if (sym) span.classList.add(scip.symbolClass(sym));
    }

    // Click handler: resolve symbol at the clicked position
    codeEl.addEventListener("click", function (e) {
      var target = e.target;
      if (target.tagName !== "SPAN" && target.tagName !== "A") return;

      var lineAttr = target.getAttribute("data-line");
      var colAttr = target.getAttribute("data-col");
      if (!lineAttr || !colAttr) return;

      var line = parseInt(lineAttr, 10);
      var col = parseInt(colAttr, 10);
      var symbol = scip.resolveSymbol(file, line, col);
      if (symbol) {
        var docPath = scip.docUrl(symbol);
        if (docPath) {
          location.hash = "#" + docPath;
        }
      }
    });

    // Hover handler: show symbol info tooltip + highlight all occurrences via CSS class
    codeEl.addEventListener("mouseover", function (e) {
      var target = e.target;
      if (target.tagName !== "SPAN") return;

      var lineAttr = target.getAttribute("data-line");
      var colAttr = target.getAttribute("data-col");
      if (!lineAttr || !colAttr) return;

      var line = parseInt(lineAttr, 10);
      var col = parseInt(colAttr, 10);
      var symbol = scip.resolveSymbol(file, line, col);
      if (symbol) {
        var info = scip.symbolInfo(symbol);
        if (info) {
          try {
            var parsed = JSON.parse(info);
            target.title = parsed.display_name || symbol;
          } catch (_) {}
        }
        target.style.cursor = "pointer";
        target.style.textDecoration = "underline";
        feHighlight(scip.symbolClass(symbol));
      }
    });

    codeEl.addEventListener("mouseout", function (e) {
      if (e.target.tagName === "SPAN") {
        e.target.style.textDecoration = "";
        e.target.style.cursor = "";
        feUnhighlight();
      }
    });
  }

  /** Enrich type links (<a class="type-link">) with SCIP symbol classes + hover tooltip. */
  _setupTypeLinkHighlighting(codeEl) {
    var scip = window.FE_SCIP;
    if (!scip) return;

    var links = codeEl.querySelectorAll("a.type-link");
    for (var i = 0; i < links.length; i++) {
      (function (a) {
        var href = a.getAttribute("href") || "";
        var docUrl = href.replace(/^#\/?/, "");
        if (!docUrl) return;

        var symbol = scip.symbolForDocUrl(docUrl);
        if (!symbol) return;

        var cls = scip.symbolClass(symbol);
        a.classList.add(cls);

        // Set hover tooltip from SCIP docs
        var info = scip.symbolInfo(symbol);
        if (info) {
          try {
            var parsed = JSON.parse(info);
            if (parsed.documentation && parsed.documentation.length > 0) {
              a.title = parsed.documentation[0].replace(/```[\s\S]*?```/g, "").trim();
            }
          } catch (_) {}
        }

        a.addEventListener("mouseenter", function () { feHighlight(cls); });
        a.addEventListener("mouseleave", feUnhighlight);
      })(links[i]);
    }
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
