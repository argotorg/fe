// <fe-code-block> — Custom element for syntax-highlighted Fe code blocks.
//
// Attributes:
//   highlighted  — content is pre-highlighted HTML (from Rust SSR)
//   lang         — language name (default "fe")
//   line-numbers — show line number gutter
//   collapsed    — start collapsed with <details>/<summary>

class FeCodeBlock extends HTMLElement {
  connectedCallback() {
    this.render();
  }

  render() {
    const highlighted = this.hasAttribute("highlighted");
    const lang = this.getAttribute("lang") || "fe";
    const showLineNumbers = this.hasAttribute("line-numbers");
    const collapsed = this.hasAttribute("collapsed");

    const wrapper = document.createElement("div");
    wrapper.className = "fe-code-block-wrapper";

    const pre = document.createElement("pre");
    pre.className = "fe-code-pre";

    const code = document.createElement("code");
    code.className = "language-" + lang;

    if (highlighted) {
      // Content is pre-rendered HTML from the Rust highlighter
      code.innerHTML = this.innerHTML;
    } else {
      // Runtime fallback: html-escape raw text content
      code.textContent = this.textContent;
    }

    // Clear original content before appending rendered version
    this.innerHTML = "";

    if (showLineNumbers) {
      var lines = code.innerHTML.split("\n");
      // Trim trailing empty line from trailing newline in source
      if (lines.length > 1 && lines[lines.length - 1] === "") {
        lines = lines.slice(0, -1);
      }
      const gutter = document.createElement("div");
      gutter.className = "fe-line-numbers";
      gutter.setAttribute("aria-hidden", "true");
      for (var i = 1; i <= lines.length; i++) {
        const span = document.createElement("span");
        span.textContent = i;
        gutter.appendChild(span);
      }
      wrapper.appendChild(gutter);
    }

    pre.appendChild(code);
    wrapper.appendChild(pre);

    if (collapsed) {
      const details = document.createElement("details");
      const summary = document.createElement("summary");
      summary.textContent = lang + " code";
      details.appendChild(summary);
      details.appendChild(wrapper);
      this.appendChild(details);
    } else {
      this.appendChild(wrapper);
    }

    // If SCIP is available, make highlighted spans interactive
    this._setupScipInteraction(code);

    // Listen for live diagnostics from LSP
    this._setupLspDiagnostics(code);
  }

  /** Add click-to-navigate on highlighted spans when ScipStore is loaded. */
  _setupScipInteraction(codeEl) {
    var scip = window.FE_SCIP;
    if (!scip) return;

    var file = this.getAttribute("data-file");
    if (!file) return;

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

    // Hover handler: show symbol info tooltip
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
      }
    });

    codeEl.addEventListener("mouseout", function (e) {
      if (e.target.tagName === "SPAN") {
        e.target.style.textDecoration = "";
        e.target.style.cursor = "";
      }
    });
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
