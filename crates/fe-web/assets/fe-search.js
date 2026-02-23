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

    var index = window.FE_DOC_INDEX;
    if (!index || !index.items) return;

    var q = query.toLowerCase();
    var matches = [];
    var items = index.items;

    for (var i = 0; i < items.length && matches.length < 15; i++) {
      var item = items[i];
      var name = (item.name || "").toLowerCase();
      var path = (item.url_path || "").toLowerCase();
      if (name.indexOf(q) !== -1 || path.indexOf(q) !== -1) {
        matches.push(item);
      }
    }

    for (var j = 0; j < matches.length; j++) {
      var m = matches[j];
      var a = document.createElement("a");
      a.className = "search-result";
      a.href = "#" + (m.url_path || "");
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
}

customElements.define("fe-search", FeSearch);
