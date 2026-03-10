// <fe-symbol-link> — Inline link to a documented Fe symbol.
//
// Usage:
//   <fe-symbol-link symbol="mylib::Game/struct">Game</fe-symbol-link>
//   <fe-symbol-link symbol="mylib::Game/struct"></fe-symbol-link>
//
// If no text content is provided, the symbol's display name is used.
// Links to the static docs site when FE_DOCS_BASE is set, otherwise
// renders as a hash link with hover info.
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

    if (!feWhenReady(this._renderLink.bind(this))) return;

    var item = feFindItem(symbolPath);
    var displayText = this._userText || (item ? item.name : symbolPath.split("::").pop().split("/")[0]);
    var docsBase = window.FE_DOCS_BASE;

    var a = document.createElement("a");
    a.className = "fe-symbol-link type-link";
    a.textContent = displayText;
    a.href = (docsBase || "") + "#" + symbolPath;

    feEnrichLink(a, symbolPath);

    // Tooltip fallback from DocIndex
    if (!a.title && item && item.docs && item.docs.summary) {
      a.title = item.docs.summary;
    }

    this.innerHTML = "";
    this.appendChild(a);
  }
}

customElements.define("fe-symbol-link", FeSymbolLink);
