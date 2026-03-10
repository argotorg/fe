// <fe-symbol-link> — Inline link to a documented Fe symbol.
//
// Usage:
//   <fe-symbol-link symbol="mylib::Game/struct">Game</fe-symbol-link>
//   <fe-symbol-link symbol="mylib::Game/struct"></fe-symbol-link>
//
// If no text content is provided, the symbol's display name is used.
// Links to the static docs site when FE_DOCS_BASE is set, otherwise
// renders as a non-navigating element with hover info.
//
// Attributes:
//   symbol — doc path (e.g. "mylib::Game/struct")

class FeSymbolLink extends HTMLElement {
  static get observedAttributes() { return ["symbol"]; }

  attributeChangedCallback(name, oldVal, newVal) {
    if (name === "symbol" && oldVal !== newVal) this._renderLink();
  }

  connectedCallback() {
    if (this._userText == null) {
      this._userText = this.textContent.trim();
    }
    this._renderLink();
  }

  _renderLink() {
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
          self._renderLink();
        });
      }
      return;
    }

    // Find the item
    var item = null;
    for (var i = 0; i < index.items.length; i++) {
      if (index.items[i].path === symbolPath) {
        item = index.items[i];
        break;
      }
    }

    var displayText = this._userText || (item ? item.name : symbolPath.split("::").pop().split("/")[0]);
    var docsBase = window.FE_DOCS_BASE;

    var a = document.createElement("a");
    a.className = "fe-symbol-link type-link";
    a.textContent = displayText;

    if (docsBase) {
      a.href = docsBase + "#" + symbolPath;
    } else {
      a.href = "#" + symbolPath;
    }

    // SCIP hover enrichment
    var scip = window.FE_SCIP;
    if (scip) {
      var sym = scip.symbolForDocUrl(symbolPath);
      if (sym) {
        var cls = scip.symbolClass(sym);
        a.classList.add(cls);

        var hash = scip.symbolHash(sym);
        a.addEventListener("mouseenter", function () { feHighlight(hash); });
        a.addEventListener("mouseleave", feUnhighlight);

        var info = scip.symbolInfo(sym);
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

    // Tooltip fallback from DocIndex
    if (!a.title && item && item.docs && item.docs.summary) {
      a.title = item.docs.summary;
    }

    this.innerHTML = "";
    this.appendChild(a);
  }
}

customElements.define("fe-symbol-link", FeSymbolLink);
