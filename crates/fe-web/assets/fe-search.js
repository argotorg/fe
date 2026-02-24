// <fe-search> — Client-side doc search with debounced substring matching.
//
// Queries window.FE_DOC_INDEX (set by the static doc site shell).
// Renders an input field and a dropdown of matching results.

class FeSearch extends HTMLElement {
  connectedCallback() {
    this._timer = null;
    this.render();
  }

  disconnectedCallback() {
    if (this._timer) clearTimeout(this._timer);
  }

  render() {
    const container = document.createElement("div");
    container.className = "fe-search-container";

    const input = document.createElement("input");
    input.type = "text";
    input.className = "fe-search-input";
    input.placeholder = "Search docs\u2026";
    input.setAttribute("aria-label", "Search documentation");

    const results = document.createElement("div");
    results.className = "fe-search-results";
    results.setAttribute("role", "listbox");

    input.addEventListener("input", () => {
      if (this._timer) clearTimeout(this._timer);
      this._timer = setTimeout(() => this.search(input.value, results), 150);
    });

    container.appendChild(input);
    container.appendChild(results);
    this.appendChild(container);
  }

  search(query, resultsEl) {
    resultsEl.innerHTML = "";
    if (!query || query.length < 2) return;

    // Try SCIP-powered search first
    var scip = window.FE_SCIP;
    if (scip) {
      try {
        var results = JSON.parse(scip.search(query));
        for (var k = 0; k < results.length; k++) {
          var r = results[k];
          var a = document.createElement("a");
          a.className = "search-result";
          a.href = "#" + (r.doc_url || "");
          a.setAttribute("role", "option");

          var badge = document.createElement("span");
          badge.className = "kind-badge";
          badge.textContent = this._scipKindName(r.kind);

          var nameEl = document.createElement("span");
          nameEl.textContent = r.display_name || "";

          a.appendChild(badge);
          a.appendChild(nameEl);
          resultsEl.appendChild(a);
        }
        return;
      } catch (_) {
        // Fall through to DocIndex search
      }
    }

    // Fallback: DocIndex search
    var index = window.FE_DOC_INDEX;
    if (!index || !index.items) return;

    // kind → URL suffix (mirrors fe-web.js ITEM_KIND_INFO)
    var KIND_SUFFIX = {
      module: "mod", function: "fn", struct: "struct", enum: "enum",
      trait: "trait", contract: "contract", type_alias: "type",
      const: "const", impl: "impl", impl_trait: "impl",
    };

    var q = query.toLowerCase();
    var matches = [];
    var items = index.items;

    for (var i = 0; i < items.length && matches.length < 15; i++) {
      var item = items[i];
      var name = (item.name || "").toLowerCase();
      var path = (item.path || "").toLowerCase();
      if (name.indexOf(q) !== -1 || path.indexOf(q) !== -1) {
        matches.push(item);
      }
    }

    for (var j = 0; j < matches.length; j++) {
      var m = matches[j];
      var suffix = KIND_SUFFIX[m.kind] || m.kind;
      var a = document.createElement("a");
      a.className = "search-result";
      a.href = "#" + m.path + "/" + suffix;
      a.setAttribute("role", "option");

      var badge = document.createElement("span");
      badge.className = "kind-badge " + (m.kind || "").toLowerCase();
      badge.textContent = m.kind || "";

      var name = document.createElement("span");
      name.textContent = m.name || "";

      a.appendChild(badge);
      a.appendChild(name);
      resultsEl.appendChild(a);
    }
  }

  _scipKindName(kind) {
    var names = {
      7: "class", 11: "enum", 12: "member", 15: "field",
      17: "fn", 26: "method", 49: "struct", 53: "trait", 54: "type",
    };
    return names[kind] || "sym";
  }
}

customElements.define("fe-search", FeSearch);
