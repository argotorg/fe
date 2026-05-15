// <fe-debug-explorer> — Cross-representation debug viewer.
//
// Two panels (three on wide screens), each with a dropdown to select
// which representation to view. Shared SCIP-style symbol classes
// enable hover cross-highlighting across any pair of representations.
//
// Data: a multi-representation SCIP index where each "file" is a
// representation (source, MIR, Sonatina IR, optimized IR, VCode,
// assembly, bytecode). Symbols shared across files provide the
// cross-level connections.
//
// Attributes:
//   src — URL to fetch the SCIP index JSON

class FeDebugExplorer extends HTMLElement {
  constructor() {
    super();
    this._data = null;
    this._activeHash = null;
  }

  connectedCallback() {
    var src = this.getAttribute("src");
    if (!src) {
      this._data = this._tryInlineData();
      if (this._data) this._render();
      return;
    }
    var self = this;
    fetch(src)
      .then(function(r) { return r.json(); })
      .then(function(data) { self._data = data; self._render(); })
      .catch(function(e) { self.textContent = "Failed to load: " + e; });
  }

  _tryInlineData() {
    var script = this.querySelector("script[type='application/json']");
    if (script) return JSON.parse(script.textContent);
    return null;
  }

  _render() {
    var d = this._data;
    if (!d || !d.files) return;

    var shadow = this.attachShadow({ mode: "open" });
    var repNames = Object.keys(d.files);
    var defaultLeft = repNames[0] || "";
    var defaultRight = repNames.length > 1 ? repNames[1] : defaultLeft;
    var defaultMiddle = repNames.length > 2 ? repNames[2] : "";

    shadow.innerHTML =
      "<style>" + _css() + "</style>" +
      '<div class="explorer">' +
        this._panelHtml("left", repNames, defaultLeft) +
        this._panelHtml("middle", repNames, defaultMiddle) +
        this._panelHtml("right", repNames, defaultRight) +
      "</div>";

    this._bindPanel(shadow, "left");
    this._bindPanel(shadow, "middle");
    this._bindPanel(shadow, "right");

    var self = this;
    shadow.addEventListener("mouseover", function(e) {
      var el = e.target.closest("[data-sym]");
      if (!el) return;
      self._highlight(shadow, el.dataset.sym);
    });
    shadow.addEventListener("mouseout", function(e) {
      var el = e.target.closest("[data-sym]");
      if (!el) return;
      self._unhighlight(shadow);
    });
  }

  _panelHtml(id, repNames, selected) {
    var opts = "";
    for (var i = 0; i < repNames.length; i++) {
      var sel = repNames[i] === selected ? " selected" : "";
      opts += '<option value="' + _esc(repNames[i]) + '"' + sel + '>' +
              _esc(_displayName(repNames[i])) + '</option>';
    }
    return '<div class="panel" data-panel="' + id + '">' +
      '<div class="panel-header">' +
        '<select class="rep-select">' + opts + '</select>' +
      '</div>' +
      '<pre><code class="panel-content"></code></pre>' +
    '</div>';
  }

  _bindPanel(shadow, id) {
    var panel = shadow.querySelector('[data-panel="' + id + '"]');
    var select = panel.querySelector(".rep-select");
    var content = panel.querySelector(".panel-content");
    var self = this;

    function update() {
      var rep = select.value;
      content.innerHTML = self._renderRep(rep);
    }
    select.addEventListener("change", update);
    update();
  }

  _renderRep(repName) {
    var file = this._data.files[repName];
    if (!file) return "";
    var code = file.contents || "";
    var occurrences = file.occurrences || [];

    // Build a map of (line, col) → { symbol, endCol }
    var spanMap = {};
    for (var i = 0; i < occurrences.length; i++) {
      var occ = occurrences[i];
      var key = occ.line + ":" + occ.col;
      spanMap[key] = { symbol: occ.symbol, endCol: occ.endCol || (occ.col + 1) };
    }

    var lines = code.split("\n");
    var out = "";
    for (var ln = 0; ln < lines.length; ln++) {
      var lineNum = ln + 1;
      var line = lines[ln];
      var annotated = this._annotateLine(line, lineNum, spanMap);
      out += '<span class="line">' + _pad(lineNum, 4) + " " + annotated + "</span>\n";
    }
    return out;
  }

  _annotateLine(line, lineNum, spanMap) {
    // Collect spans on this line
    var spans = [];
    for (var key in spanMap) {
      var parts = key.split(":");
      if (parseInt(parts[0]) === lineNum) {
        var col = parseInt(parts[1]);
        spans.push({ col: col, endCol: spanMap[key].endCol, symbol: spanMap[key].symbol });
      }
    }
    if (spans.length === 0) return _esc(line);

    spans.sort(function(a, b) { return a.col - b.col; });

    var result = "";
    var pos = 0;
    for (var i = 0; i < spans.length; i++) {
      var s = spans[i];
      var startIdx = s.col - 1;
      var endIdx = s.endCol - 1;
      if (startIdx < 0) startIdx = 0;
      if (endIdx > line.length) endIdx = line.length;
      if (startIdx > pos) {
        result += _esc(line.substring(pos, startIdx));
      }
      var hash = _djb2(s.symbol);
      var text = line.substring(startIdx, endIdx);
      result += '<span class="sym sym-' + hash + '" data-sym="' + hash + '">' +
                _esc(text) + '</span>';
      pos = endIdx;
    }
    if (pos < line.length) {
      result += _esc(line.substring(pos));
    }
    return result;
  }

  _highlight(root, hash) {
    this._unhighlight(root);
    this._activeHash = hash;
    root.querySelectorAll(".sym-" + hash).forEach(function(el) {
      el.classList.add("hl");
    });
  }

  _unhighlight(root) {
    if (!this._activeHash) return;
    root.querySelectorAll(".sym-" + this._activeHash).forEach(function(el) {
      el.classList.remove("hl");
    });
    this._activeHash = null;
  }
}

function _djb2(str) {
  var hash = 5381;
  for (var i = 0; i < str.length; i++) {
    hash = ((hash << 5) + hash + str.charCodeAt(i)) & 0xffffffff;
  }
  return ("000000" + (hash >>> 0).toString(16)).slice(-6);
}

function _esc(s) {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}

function _pad(n, w) {
  var s = "" + n;
  while (s.length < w) s = " " + s;
  return s;
}

function _displayName(repName) {
  var names = {
    "source": "Fe Source",
    "mir": "MIR",
    "sonatina_ir": "Sonatina IR",
    "sonatina_ir_opt": "Sonatina IR (optimized)",
    "vcode": "VCode",
    "asm": "EVM Assembly",
    "bytecode": "EVM Bytecode"
  };
  return names[repName] || repName;
}

function _css() {
  return `
:host { display: block; width: 100%; height: 100%; }
.explorer {
  display: grid;
  grid-template-columns: 1fr 1fr;
  height: 100%;
  gap: 2px;
  background: #11111b;
  font-family: 'JetBrains Mono', 'Fira Code', 'Cascadia Code', monospace;
}
@media (min-width: 1400px) {
  .explorer { grid-template-columns: 1fr 1fr 1fr; }
  .panel[data-panel="middle"] { display: block; }
}
.panel { background: #1e1e2e; overflow: auto; display: flex; flex-direction: column; }
.panel[data-panel="middle"] { display: none; }
.panel-header {
  position: sticky; top: 0; z-index: 1;
  background: #1e1e2e; padding: 6px 8px; border-bottom: 1px solid #313244;
}
.rep-select {
  background: #313244; color: #cdd6f4; border: 1px solid #45475a;
  border-radius: 4px; padding: 4px 8px; font-size: 11px;
  font-family: inherit; cursor: pointer; width: 100%;
}
.rep-select:focus { outline: 1px solid #89b4fa; }
pre { font-size: 12px; line-height: 1.6; color: #cdd6f4; margin: 0; padding: 8px; flex: 1; }
code { display: block; }
.line { display: block; padding: 0 4px; border-radius: 2px; }
.sym {
  transition: background 0.12s ease;
  border-radius: 2px; cursor: default;
}
.sym.hl {
  background: rgba(137, 180, 250, 0.2);
  outline: 1px solid rgba(137, 180, 250, 0.3);
}
  `;
}

customElements.define("fe-debug-explorer", FeDebugExplorer);
