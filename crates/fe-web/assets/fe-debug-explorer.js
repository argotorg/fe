// <fe-debug-explorer> — Cross-representation debug info viewer.
//
// Renders 4 panels (source, MIR, Sonatina IR, bytecode) with
// CSS-driven hover cross-highlighting. Hovering over any region
// highlights all corresponding regions across all panels.
//
// Usage:
//   <fe-debug-explorer data-payload="url-or-inline-json"></fe-debug-explorer>
//
// Payload JSON shape:
// {
//   "source": { "path": "test.fe", "code": "fn add(...) { ... }" },
//   "mir": "bb0:\n  ...",
//   "sonatina_ir": "func %add:\n  ...",
//   "groups": [
//     {
//       "id": 0,
//       "source_lines": [3, 4],
//       "mir_stmts": [{ "func": 1, "block": 0, "stmt": 2 }],
//       "ir_insts": [{ "func": 1, "inst": 5 }],
//       "pc_ranges": [{ "start": 0, "end": 10, "func_name": "add" }]
//     }
//   ]
// }

class FeDebugExplorer extends HTMLElement {
  constructor() {
    super();
    this._data = null;
    this._activeGroups = new Set();
  }

  connectedCallback() {
    var src = this.getAttribute("data-payload");
    if (!src) return;

    // Inline JSON or fetch URL
    if (src.trim().startsWith("{")) {
      this._data = JSON.parse(src);
      this._render();
    } else {
      fetch(src)
        .then(r => r.json())
        .then(data => { this._data = data; this._render(); })
        .catch(e => { this.textContent = "Failed to load payload: " + e; });
    }
  }

  _render() {
    var d = this._data;
    if (!d) return;

    var shadow = this.attachShadow({ mode: "open" });
    shadow.innerHTML = `
      <style>${_explorerCSS()}</style>
      <div class="explorer">
        <div class="panel" id="source-panel">
          <h3>Fe Source</h3>
          <pre><code>${this._renderSource(d)}</code></pre>
        </div>
        <div class="panel" id="mir-panel">
          <h3>MIR</h3>
          <pre><code>${_escHtml(d.mir || "")}</code></pre>
        </div>
        <div class="panel" id="ir-panel">
          <h3>Sonatina IR</h3>
          <pre><code>${_escHtml(d.sonatina_ir || "")}</code></pre>
        </div>
        <div class="panel" id="bytecode-panel">
          <h3>EVM Bytecode</h3>
          <pre><code>${this._renderBytecode(d)}</code></pre>
        </div>
      </div>
    `;

    var self = this;
    shadow.addEventListener("mouseover", function(e) {
      var el = e.target.closest("[data-groups]");
      if (!el) return;
      var groups = el.dataset.groups.split(" ").filter(Boolean);
      if (!groups.length) return;
      self._clearHighlights(shadow);
      groups.forEach(function(g) { self._activeGroups.add(g); });
      self._applyHighlights(shadow);
    });

    shadow.addEventListener("mouseout", function(e) {
      var el = e.target.closest("[data-groups]");
      if (!el) return;
      self._clearHighlights(shadow);
    });
  }

  _renderSource(d) {
    if (!d.source || !d.source.code) return "";
    var lines = d.source.code.split("\n");
    var lineGroups = this._buildLineGroupMap(d);
    var out = "";
    for (var i = 0; i < lines.length; i++) {
      var lineNum = i + 1;
      var gClasses = (lineGroups[lineNum] || []).map(function(g) { return "g-" + g; }).join(" ");
      var dataAttr = gClasses ? ' data-groups="' + gClasses + '"' : "";
      var cls = "line" + (gClasses ? " " + gClasses : "");
      out += '<span class="' + cls + '"' + dataAttr + '>';
      out += _pad(lineNum, 3) + " " + _escHtml(lines[i]);
      out += "</span>\n";
    }
    return out;
  }

  _renderBytecode(d) {
    if (!d.groups) return "";
    var pcGroups = {};
    d.groups.forEach(function(group) {
      (group.pc_ranges || []).forEach(function(pc) {
        var key = pc.start + "-" + pc.end;
        if (!pcGroups[key]) pcGroups[key] = { pc: pc, groups: [] };
        pcGroups[key].groups.push(group.id);
      });
    });
    var entries = Object.values(pcGroups);
    entries.sort(function(a, b) { return a.pc.start - b.pc.start; });
    var out = "";
    entries.forEach(function(e) {
      var gClasses = e.groups.map(function(g) { return "g-" + g; }).join(" ");
      var dataAttr = gClasses ? ' data-groups="' + gClasses + '"' : "";
      var cls = "line" + (gClasses ? " " + gClasses : "");
      var hex = function(n) { return ("0000" + n.toString(16)).slice(-4); };
      var label = e.pc.func_name || "";
      out += '<span class="' + cls + '"' + dataAttr + '>';
      out += "  [" + hex(e.pc.start) + ".." + hex(e.pc.end) + ") " + _escHtml(label);
      out += "</span>\n";
    });
    return out;
  }

  _buildLineGroupMap(d) {
    var map = {};
    if (!d.groups) return map;
    d.groups.forEach(function(group) {
      (group.source_lines || []).forEach(function(line) {
        if (!map[line]) map[line] = [];
        map[line].push(group.id);
      });
    });
    return map;
  }

  _applyHighlights(root) {
    this._activeGroups.forEach(function(g) {
      root.querySelectorAll("." + g).forEach(function(el) {
        el.classList.add("highlighted");
      });
    });
  }

  _clearHighlights(root) {
    this._activeGroups.forEach(function(g) {
      root.querySelectorAll("." + g).forEach(function(el) {
        el.classList.remove("highlighted");
      });
    });
    this._activeGroups.clear();
  }
}

function _escHtml(s) {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

function _pad(n, w) {
  var s = "" + n;
  while (s.length < w) s = " " + s;
  return s;
}

function _explorerCSS() {
  return `
:host { display: block; width: 100%; height: 100%; }
.explorer {
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-template-rows: 1fr 1fr;
  height: 100%;
  gap: 2px;
  background: #11111b;
  font-family: 'JetBrains Mono', 'Fira Code', 'Cascadia Code', monospace;
}
.panel {
  background: #1e1e2e;
  overflow: auto;
  padding: 8px;
}
.panel h3 {
  color: #89b4fa;
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 0.1em;
  margin-bottom: 8px;
  position: sticky;
  top: 0;
  background: #1e1e2e;
  padding: 4px 0;
  z-index: 1;
}
pre { font-size: 12px; line-height: 1.6; color: #cdd6f4; margin: 0; }
code { display: block; }
.line {
  display: block;
  padding: 0 4px;
  border-left: 2px solid transparent;
  transition: background 0.12s ease, border-color 0.12s ease;
  border-radius: 2px;
  cursor: default;
}
.line.highlighted {
  background: rgba(137, 180, 250, 0.15);
  border-left-color: rgba(137, 180, 250, 0.5);
}
.line[data-groups]:hover {
  background: rgba(137, 180, 250, 0.08);
}
  `;
}

customElements.define("fe-debug-explorer", FeDebugExplorer);
