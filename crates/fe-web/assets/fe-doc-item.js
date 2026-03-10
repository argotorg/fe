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

    if (!feWhenReady(this._renderItem.bind(this))) return;

    var item = feFindItem(symbolPath);
    if (!item) {
      this.innerHTML = "<span class=\"fe-doc-item-error\">Item not found: " +
        feEscapeHtml(symbolPath) + "</span>";
      return;
    }

    // Use the full renderer from fe-web.js if available
    if (window._feRenderDocItem) {
      this.innerHTML = window._feRenderDocItem(item);
      this._refreshCodeBlocks();
      return;
    }

    // Fallback: minimal rendering for standalone component bundle usage.
    // Re-render when fe-web.js finishes loading (provides the full renderer).
    this._renderFallback(item);
    if (!this._awaitingRenderer) {
      this._awaitingRenderer = true;
      var self = this;
      document.addEventListener("fe-web-ready", function onReady() {
        document.removeEventListener("fe-web-ready", onReady);
        self._awaitingRenderer = false;
        if (window._feRenderDocItem) self._renderItem();
      });
    }
  }

  _renderFallback(item) {
    var compact = this.hasAttribute("compact");
    var showSource = this.hasAttribute("show-source");

    var html = "<div class=\"fe-doc-item\">";

    html += "<div class=\"fe-doc-item-header\">";
    html += "<span class=\"fe-doc-item-kind\">" + feEscapeHtml(item.kind) + "</span> ";
    html += "<span class=\"fe-doc-item-name\">" + feEscapeHtml(item.name) + "</span>";
    html += "</div>";

    if (item.rich_signature && item.rich_signature.length > 0) {
      var sigEl = document.createElement("fe-signature");
      sigEl.setAttribute("data", JSON.stringify(item.rich_signature));
      html += "<div class=\"fe-doc-item-sig\">" + sigEl.outerHTML + "</div>";
    } else if (item.signature) {
      html += "<div class=\"fe-doc-item-sig\"><code class=\"fe-sig\">" +
        feEscapeHtml(item.signature) + "</code></div>";
    }

    if (item.docs) {
      if (item.docs.html_summary) {
        html += "<p class=\"fe-doc-item-summary\">" + item.docs.html_summary + "</p>";
      } else if (item.docs.summary) {
        html += "<p class=\"fe-doc-item-summary\">" + feEscapeHtml(item.docs.summary) + "</p>";
      }
      if (!compact && item.docs.html_body) {
        html += "<div class=\"fe-doc-item-body\">" + item.docs.html_body + "</div>";
      } else if (!compact && item.docs.body) {
        html += "<div class=\"fe-doc-item-body\">" + feEscapeHtml(item.docs.body) + "</div>";
      }
    }

    if (!compact && item.children && item.children.length > 0) {
      html += "<div class=\"fe-doc-item-children\"><h4>Members</h4>";
      html += "<dl class=\"fe-doc-item-members\">";
      for (var ci = 0; ci < item.children.length; ci++) {
        var child = item.children[ci];
        html += "<dt><code>" + feEscapeHtml(child.signature || child.name) + "</code></dt>";
        if (child.docs && child.docs.summary) {
          html += "<dd>" + feEscapeHtml(child.docs.summary) + "</dd>";
        }
      }
      html += "</dl></div>";
    }

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

    var docsBase = window.FE_DOCS_BASE;
    if (docsBase) {
      html += "<a class=\"fe-doc-item-link\" href=\"" +
        feEscapeHtml(docsBase + "#" + item.path + "/" + item.kind) + "\">View full docs</a>";
    }

    this.innerHTML = html;
  }

  _refreshCodeBlocks() {
    var blocks = this.querySelectorAll("fe-code-block");
    for (var i = 0; i < blocks.length; i++) {
      if (blocks[i].refresh) blocks[i].refresh();
    }
  }
}

customElements.define("fe-doc-item", FeDocItem);
