// <fe-origin-trace> — Interactive trace-through graph viewer.
//
// The component expects `window.FE_ORIGIN_TRACE_DATA` to contain a derived trace
// view model. Regions across panels are connected by CSS classes named
// `trace-c-*`, mirroring the SCIP def/ref highlighting strategy used by
// <fe-code-block>.

(function () {
  "use strict";

  function el(tag, cls, text) {
    var node = document.createElement(tag);
    if (cls) node.className = cls;
    if (text !== undefined && text !== null) node.textContent = String(text);
    return node;
  }

  function traceClasses(node) {
    return Array.prototype.filter.call(node.classList || [], function (name) {
      return name.indexOf("trace-c-") === 0;
    });
  }

  function keyClass(key) {
    var hash = 2166136261;
    for (var i = 0; i < key.length; i++) {
      hash ^= key.charCodeAt(i);
      hash = Math.imul(hash, 16777619);
    }
    return "trace-key-" + (hash >>> 0).toString(16);
  }

  class FeOriginTrace extends HTMLElement {
    connectedCallback() {
      if (!this.shadowRoot) this.attachShadow({ mode: "open" });
      this._data = window.FE_ORIGIN_TRACE_DATA || {};
      this._selected = [];
      this._render();
    }

    _render() {
      var data = this._data;
      this.shadowRoot.textContent = "";
      this.shadowRoot.append(el("style", "", this._style()));

      var page = el("div", "trace-page");
      var header = el("header", "trace-header");
      header.append(
        el("p", "eyebrow", "Fe origin trace"),
        el("h1", "", "Trace-through graph"),
        el(
          "p",
          "subtitle",
          "Derived browser view over compiler trace facts. Hover or click any highlighted region to select its transitive origin closure across source, HIR, MIR, Sonatina, and bytecode."
        )
      );
      page.append(header);

      var cards = el("section", "cards");
      this._card(cards, "data source", data.metadata && data.metadata.data_source);
      this._card(cards, "facts", data.counts && data.counts.facts);
      this._card(cards, "closures", (data.closures || []).length);
      this._card(cards, "source confidence", data.source && data.source.confidence);
      if (data.salsa) {
        this._card(cards, "salsa mode", data.salsa.mode);
        this._card(cards, "query execs", data.salsa.will_execute);
        this._card(cards, "memo reuse", data.salsa.memo_reuse);
        this._card(cards, "render ms", data.salsa.elapsed_ms);
      }
      page.append(cards);

      var workspace = el("main", "workspace");
      workspace.append(this._sourcePanel(data.source || {}));
      (data.panels || []).forEach(function (panel) {
        workspace.append(this._panel(panel));
      }, this);
      workspace.append(this._detailPanel());
      page.append(workspace);

      var notes = el("ul", "notes");
      (data.notes || []).forEach(function (note) {
        notes.append(el("li", "", note));
      });
      page.append(notes);
      this.shadowRoot.append(page);
      this._installInteractions();
      this._renderDetail(null);
    }

    _card(parent, label, value) {
      var card = el("div", "card");
      card.append(el("span", "", label), el("b", "", value == null ? "unknown" : value));
      parent.append(card);
    }

    _sourcePanel(source) {
      var panel = el("section", "panel source-panel");
      panel.append(this._panelHead("Source", source.display_name || "source"));
      var body = el("div", "source-lines");
      (source.lines || []).forEach(function (line) {
        var row = el("div", "source-line trace-region " + (line.classes || []).join(" "));
        row.dataset.traceLabel = "source line " + line.number;
        row.append(el("span", "ln", line.number), el("code", "", line.text));
        body.append(row);
      });
      panel.append(body);
      return panel;
    }

    _panel(panelData) {
      var panel = el("section", "panel");
      panel.append(this._panelHead(panelData.title, panelData.summary));
      var rows = el("div", "rows");
      (panelData.rows || []).forEach(function (rowData) {
        var row = el("button", "trace-row trace-region " + (rowData.classes || []).join(" "));
        row.type = "button";
        if (rowData.key) {
          row.dataset.originKey = rowData.key;
          row.classList.add(keyClass(rowData.key));
        }
        row.dataset.traceLabel = rowData.label || rowData.text || rowData.key || "";
        row.append(
          el("span", "row-label", rowData.label),
          el("span", "row-meta", rowData.meta),
          el("span", "row-text", rowData.text)
        );
        rows.append(row);
      });
      panel.append(rows);
      return panel;
    }

    _panelHead(title, summary) {
      var head = el("div", "panel-head");
      head.append(el("h2", "", title || "Panel"));
      if (summary) head.append(el("p", "", summary));
      return head;
    }

    _detailPanel() {
      var panel = el("section", "panel detail-panel");
      panel.append(this._panelHead("Selected Closure", "Click a highlighted region"));
      panel.append(el("div", "detail", ""));
      return panel;
    }

    _installInteractions() {
      var root = this.shadowRoot;
      root.addEventListener("mouseover", function (event) {
        var row = event.target.closest && event.target.closest(".trace-region");
        if (!row || (event.relatedTarget && row.contains(event.relatedTarget))) return;
        this._setHover(traceClasses(row), true);
      }.bind(this));
      root.addEventListener("mouseout", function (event) {
        var row = event.target.closest && event.target.closest(".trace-region");
        if (!row || (event.relatedTarget && row.contains(event.relatedTarget))) return;
        this._setHover(traceClasses(row), false);
      }.bind(this));
      root.addEventListener("click", function (event) {
        var row = event.target.closest && event.target.closest(".trace-region");
        if (!row) return;
        var groups = traceClasses(row);
        this._select(groups);
      }.bind(this));
    }

    _setHover(groups, on) {
      this._forGroups(groups, function (node) {
        node.classList.toggle("trace-hover", on);
      });
    }

    _select(groups) {
      this._forGroups(this._selected, function (node) {
        node.classList.remove("trace-selected");
      });
      this._selected = groups.slice();
      this._forGroups(this._selected, function (node) {
        node.classList.add("trace-selected");
      });
      this._renderDetail(this._selected);
    }

    _forGroups(groups, f) {
      var seen = Object.create(null);
      groups.forEach(function (group) {
        if (!group || seen[group]) return;
        seen[group] = true;
        this.shadowRoot.querySelectorAll("." + group).forEach(f);
      }, this);
    }

    _renderDetail(groups) {
      var detail = this.shadowRoot.querySelector(".detail");
      if (!detail) return;
      detail.textContent = "";
      if (!groups || groups.length === 0) {
        detail.append(el("p", "muted", "No closure selected."));
        return;
      }
      var closuresByClass = Object.create(null);
      (this._data.closures || []).forEach(function (closure) {
        closuresByClass[closure.class_name] = closure;
      });
      groups.forEach(function (group) {
        var closure = closuresByClass[group];
        if (!closure) return;
        var box = el("div", "closure-card");
        box.append(el("h3", "", closure.label));
        (closure.edges || []).forEach(function (edge) {
          var row = el("div", "edge");
          row.append(
            el("span", "edge-label", edge.label),
            el("span", "edge-text", edge.from + " -> " + edge.to)
          );
          box.append(row);
        });
        if ((closure.source_spans || []).length) {
          box.append(el("h4", "", "Source spans"));
          closure.source_spans.forEach(function (span) {
            box.append(el("div", "span-line", span.lines + " " + span.origin));
          });
        }
        detail.append(box);
      });
    }

    _style() {
      return `
:host { display:block; color:#f5f1df; background:#11130f; font:14px/1.45 "JetBrains Mono", "Fira Code", ui-monospace, monospace; }
* { box-sizing:border-box; }
.trace-page { min-height:100vh; background:radial-gradient(circle at 12% 0%, #344021 0, transparent 36rem), linear-gradient(140deg,#10140f,#18150f 58%,#101820); }
.trace-header { padding:28px 32px 16px; border-bottom:1px solid #333b2b; }
.eyebrow { margin:0 0 8px; color:#a7d982; text-transform:uppercase; letter-spacing:.16em; font-size:12px; }
h1 { margin:0; font:700 32px/1.1 Georgia,serif; }
.subtitle { max-width:1050px; margin:10px 0 0; color:#b8b098; }
.cards { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:12px; padding:16px 32px; }
.card,.panel { background:color-mix(in srgb,#191d16,transparent 6%); border:1px solid #343b2e; border-radius:18px; box-shadow:0 18px 48px #0007; }
.card { padding:13px 15px; min-width:0; }
.card span { display:block; color:#aaa58e; font-size:12px; text-transform:uppercase; letter-spacing:.08em; }
.card b { display:block; margin-top:4px; color:#f3bd55; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.workspace { display:grid; grid-template-columns:minmax(330px,1.15fr) repeat(3,minmax(260px,.8fr)); gap:14px; padding:0 32px 28px; align-items:start; }
.panel { overflow:hidden; min-height:260px; }
.source-panel { grid-row:span 2; }
.detail-panel { grid-column:span 2; }
.panel-head { padding:13px 15px; border-bottom:1px solid #343b2e; }
.panel-head h2 { margin:0; color:#9ad97f; font-size:13px; text-transform:uppercase; letter-spacing:.14em; }
.panel-head p { margin:5px 0 0; color:#8f957d; font-size:12px; }
.source-lines,.rows,.detail { max-height:520px; overflow:auto; }
.source-line { display:grid; grid-template-columns:48px 1fr; gap:10px; padding:0 14px; white-space:pre; cursor:pointer; }
.source-line code { color:#ede5c9; font:inherit; }
.ln { color:#6f765f; text-align:right; user-select:none; }
.trace-row { display:grid; grid-template-columns:minmax(70px,.45fr) minmax(75px,.45fr) minmax(140px,1fr); width:100%; gap:8px; padding:9px 12px; border:0; border-bottom:1px solid #2c3229; background:transparent; color:#e7dec3; text-align:left; font:inherit; cursor:pointer; }
.trace-row:hover,.source-line:hover { background:#262817; }
.row-label { color:#f0bd58; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.row-meta { color:#7fb5ff; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.row-text { color:#bdb59d; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
[class*="trace-c-"] { transition:background .16s ease-out, color .16s ease-out, outline-color .16s ease-out; }
.trace-hover { background:#2a2b17 !important; outline:1px solid #7d6a31; outline-offset:-1px; }
.trace-selected { background:#4d3416 !important; color:#fff2be !important; outline:1px solid #f0bd58; outline-offset:-1px; }
.detail { padding:14px 15px; }
.muted { color:#8f957d; margin:0; }
.closure-card { padding:12px; border:1px solid #30382b; border-radius:12px; background:#11160f; margin-bottom:12px; }
.closure-card h3 { margin:0 0 8px; color:#f0bd58; font-size:14px; }
.closure-card h4 { margin:12px 0 6px; color:#9ad97f; font-size:12px; text-transform:uppercase; letter-spacing:.1em; }
.edge,.span-line { display:grid; grid-template-columns:145px 1fr; gap:8px; padding:4px 0; color:#aeb59b; }
.edge-label { color:#7fb5ff; }
.edge-text,.span-line { overflow-wrap:anywhere; }
.notes { margin:0; padding:0 32px 30px 52px; color:#aaa58e; }
.notes li { margin:6px 0; }
@media (max-width:1250px) { .workspace,.cards { grid-template-columns:1fr 1fr; } .source-panel,.detail-panel { grid-column:span 2; } }
@media (max-width:760px) { .workspace,.cards { grid-template-columns:1fr; padding-left:14px; padding-right:14px; } .source-panel,.detail-panel { grid-column:span 1; } .trace-header { padding-left:14px; padding-right:14px; } .notes { padding-left:32px; padding-right:14px; } }
`;
    }
  }

  if (!customElements.get("fe-origin-trace")) {
    customElements.define("fe-origin-trace", FeOriginTrace);
  }
})();
