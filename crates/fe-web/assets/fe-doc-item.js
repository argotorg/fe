// <fe-doc-item> — Renders a documentation item from FE_DOC_INDEX.
//
// Delegates to the same renderDocItem() used by the static site, so the
// output is identical.  Falls back to a minimal rendering if fe-web.js
// hasn't loaded (e.g. when only the component bundle is used without the
// full app JS).
//
// Usage:
//   <fe-doc-item symbol="mylib::Game/struct"></fe-doc-item>
//
// Attributes:
//   symbol     — doc path to look up (e.g. "mylib::Game/struct")
//   show-source — show the full source text if available
//   compact    — render a condensed version (signature + summary only)

class FeDocItem extends HTMLElement {
  static get observedAttributes() { return ["symbol"]; }

  attributeChangedCallback(name, oldVal, newVal) {
    if (name === "symbol" && oldVal !== newVal) this._renderItem();
  }

  connectedCallback() {
    this._renderItem();
  }

  _renderItem() {
    var symbolPath = this.getAttribute("symbol");
    if (!symbolPath) return;

    var index = window.FE_DOC_INDEX;
    if (!index || !index.items) {
      if (!this._waiting) {
        this._waiting = true;
        var self = this;
        document.addEventListener("fe-web-ready", function onReady() {
          document.removeEventListener("fe-web-ready", onReady);
          self._waiting = false;
          self._renderItem();
        });
      }
      return;
    }

    var item = null;
    for (var i = 0; i < index.items.length; i++) {
      if (index.items[i].path === symbolPath) {
        item = index.items[i];
        break;
      }
    }

    if (!item) {
      this.innerHTML = "<span class=\"fe-doc-item-error\">Item not found: " +
        _feEscapeHtml(symbolPath) + "</span>";
      return;
    }

    // Use the full renderer from fe-web.js if available
    if (window._feRenderDocItem) {
      this.innerHTML = window._feRenderDocItem(item);
      this._setupScipInteraction();
      return;
    }

    // Fallback: minimal rendering for standalone component bundle usage
    this._renderFallback(item);
  }

  _renderFallback(item) {
    var compact = this.hasAttribute("compact");
    var showSource = this.hasAttribute("show-source");

    var html = "<div class=\"fe-doc-item\">";

    // Header: kind badge + name
    html += "<div class=\"fe-doc-item-header\">";
    html += "<span class=\"fe-doc-item-kind\">" + _feEscapeHtml(item.kind) + "</span> ";
    html += "<span class=\"fe-doc-item-name\">" + _feEscapeHtml(item.name) + "</span>";
    html += "</div>";

    // Signature (prefer rich_signature, fall back to plain)
    if (item.rich_signature && item.rich_signature.length > 0) {
      var sigEl = document.createElement("fe-signature");
      sigEl.setAttribute("data", JSON.stringify(item.rich_signature));
      html += "<div class=\"fe-doc-item-sig\">" + sigEl.outerHTML + "</div>";
    } else if (item.signature) {
      html += "<div class=\"fe-doc-item-sig\"><code class=\"fe-sig\">" +
        _feEscapeHtml(item.signature) + "</code></div>";
    }

    // Documentation
    if (item.docs) {
      if (item.docs.summary) {
        html += "<p class=\"fe-doc-item-summary\">" + item.docs.summary + "</p>";
      }
      if (!compact && item.docs.body) {
        html += "<div class=\"fe-doc-item-body\">" + item.docs.body + "</div>";
      }
    }

    // Children (methods, fields, variants) — skip in compact mode
    if (!compact && item.children && item.children.length > 0) {
      html += "<div class=\"fe-doc-item-children\">";
      html += "<h4>Members</h4>";
      html += "<dl class=\"fe-doc-item-members\">";
      for (var ci = 0; ci < item.children.length; ci++) {
        var child = item.children[ci];
        html += "<dt><code>" + _feEscapeHtml(child.signature || child.name) + "</code></dt>";
        if (child.docs && child.docs.summary) {
          html += "<dd>" + child.docs.summary + "</dd>";
        }
      }
      html += "</dl></div>";
    }

    // Source text
    if (showSource && item.source_text) {
      html += "<div class=\"fe-doc-item-source\">";
      var cb = document.createElement("fe-code-block");
      cb.setAttribute("line-numbers", "");
      if (item.source && item.source.display_file) {
        cb.setAttribute("data-file", item.source.display_file);
      }
      cb.textContent = item.source_text;
      html += cb.outerHTML;
      html += "</div>";
    }

    html += "</div>";

    // Link to full docs if data-docs base is set
    var docsBase = window.FE_DOCS_BASE;
    if (docsBase) {
      html += "<a class=\"fe-doc-item-link\" href=\"" +
        _feEscapeHtml(docsBase + "#" + item.path + "/" + item.kind) + "\">View full docs</a>";
    }

    this.innerHTML = html;
  }

  /** Wire up SCIP interaction on any code blocks we just rendered. */
  _setupScipInteraction() {
    var blocks = this.querySelectorAll("fe-code-block");
    for (var i = 0; i < blocks.length; i++) {
      if (blocks[i].refresh) blocks[i].refresh();
    }
  }
}

function _feEscapeHtml(s) {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}

customElements.define("fe-doc-item", FeDocItem);
