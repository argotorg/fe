// ScipStore — Pure-JS symbol index built from pre-processed SCIP JSON.
//
// The server (Rust) converts the SCIP protobuf into a compact JSON object
// with two keys:
//   symbols: { [scip_symbol]: { name, kind, docs?, enclosing?, doc_url? } }
//   files:   { [path]: [ { line, cs, ce, sym, def? }, ... ] }
//
// Usage:
//   window.FE_SCIP = new ScipStore(window.FE_SCIP_DATA);

// SCIP symbol hover highlighting.
// Colors come from CSS custom properties (--hl-ref-bg, --hl-def-bg,
// --hl-def-underline) defined in :root so they stay in sync with the theme.
// Setting element.style.* directly lets the CSS transition on [class*="sym-"]
// interpolate between transparent ↔ colored.
var _defaultHighlightHash = null;
var _activeHighlightHash = null;

// Read highlight colors from CSS custom properties, with fallbacks.
function _hlColor(prop, fallback) {
  var v = getComputedStyle(document.documentElement).getPropertyValue(prop);
  return v && v.trim() ? v.trim() : fallback;
}

function feHighlight(symHash) {
  if (_activeHighlightHash && _activeHighlightHash !== symHash) {
    _setHighlightStyles(_activeHighlightHash, false);
  }
  _activeHighlightHash = symHash;
  if (symHash) _setHighlightStyles(symHash, true);
}

function _applyHighlightTo(root, symHash, refBg, defBg, defUl, on) {
  var all = root.querySelectorAll(".sym-" + symHash);
  var defs = root.querySelectorAll(".sym-d-" + symHash);
  for (var i = 0; i < all.length; i++) {
    all[i].style.background = refBg;
    all[i].style.borderRadius = on ? "2px" : "";
  }
  for (var j = 0; j < defs.length; j++) {
    defs[j].style.background = defBg;
    defs[j].style.textDecoration = on ? "underline" : "";
    defs[j].style.textDecorationColor = defUl;
    defs[j].style.textUnderlineOffset = on ? "2px" : "";
  }
}

function _setHighlightStyles(symHash, on) {
  var refBg  = on ? _hlColor("--hl-ref-bg",       "rgba(99,102,241,0.10)") : "";
  var defBg  = on ? _hlColor("--hl-def-bg",        "rgba(99,102,241,0.18)") : "";
  var defUl  = on ? _hlColor("--hl-def-underline",  "rgba(99,102,241,0.5)") : "";
  // Search light DOM
  _applyHighlightTo(document, symHash, refBg, defBg, defUl, on);
  // Search shadow roots of code blocks
  var blocks = document.querySelectorAll("fe-code-block");
  for (var i = 0; i < blocks.length; i++) {
    if (blocks[i].shadowRoot) {
      _applyHighlightTo(blocks[i].shadowRoot, symHash, refBg, defBg, defUl, on);
    }
  }
}

function feUnhighlight() {
  if (_activeHighlightHash) {
    _setHighlightStyles(_activeHighlightHash, false);
    _activeHighlightHash = null;
  }
  if (_defaultHighlightHash) {
    feHighlight(_defaultHighlightHash);
  }
}
// Set the ambient/default symbol highlight for the current page.
// feUnhighlight() restores this instead of fully clearing.
function feSetDefaultHighlight(symHash) {
  _defaultHighlightHash = symHash;
  if (symHash) feHighlight(symHash);
}
function feClearDefaultHighlight() {
  _defaultHighlightHash = null;
  feUnhighlight();
}

function ScipStore(data) {
  this._symbols = data.symbols || {};
  this._files = data.files || {};

  // Build name → [symbol] index for search
  this._byName = {};
  var syms = this._symbols;
  for (var sym in syms) {
    if (!syms.hasOwnProperty(sym)) continue;
    var name = syms[sym].name || "";
    var lower = name.toLowerCase();
    if (!this._byName[lower]) this._byName[lower] = [];
    this._byName[lower].push(sym);
  }
}

// Resolve a symbol at (file, line, col). Returns symbol string or null.
ScipStore.prototype.resolveSymbol = function (file, line, col) {
  var occs = this._files[file];
  if (!occs) return null;
  // Binary search by line, then linear scan within line
  var lo = 0, hi = occs.length - 1;
  while (lo <= hi) {
    var mid = (lo + hi) >>> 1;
    if (occs[mid].line < line) lo = mid + 1;
    else if (occs[mid].line > line) hi = mid - 1;
    else { lo = mid; break; }
  }
  // Scan all occurrences on this line
  for (var i = lo; i < occs.length && occs[i].line === line; i++) {
    if (col >= occs[i].cs && col < occs[i].ce) return occs[i].sym;
  }
  // Also scan backwards in case lo overshot
  for (var j = lo - 1; j >= 0 && occs[j].line === line; j--) {
    if (col >= occs[j].cs && col < occs[j].ce) return occs[j].sym;
  }
  return null;
};

// Resolve an occurrence at (file, line, col). Returns {sym, def} or null.
// Like resolveSymbol but also exposes the definition flag for role-aware styling.
ScipStore.prototype.resolveOccurrence = function (file, line, col) {
  var occs = this._files[file];
  if (!occs) return null;
  var lo = 0, hi = occs.length - 1;
  while (lo <= hi) {
    var mid = (lo + hi) >>> 1;
    if (occs[mid].line < line) lo = mid + 1;
    else if (occs[mid].line > line) hi = mid - 1;
    else { lo = mid; break; }
  }
  for (var i = lo; i < occs.length && occs[i].line === line; i++) {
    if (col >= occs[i].cs && col < occs[i].ce) {
      return { sym: occs[i].sym, def: !!occs[i].def };
    }
  }
  for (var j = lo - 1; j >= 0 && occs[j].line === line; j--) {
    if (col >= occs[j].cs && col < occs[j].ce) {
      return { sym: occs[j].sym, def: !!occs[j].def };
    }
  }
  return null;
};

// Return JSON string with symbol metadata, or null.
ScipStore.prototype.symbolInfo = function (symbol) {
  var info = this._symbols[symbol];
  if (!info) return null;
  return JSON.stringify({
    symbol: symbol,
    display_name: info.name,
    kind: info.kind,
    documentation: info.docs || [],
    enclosing_symbol: info.enclosing || "",
  });
};

// Fuzzy match helper: returns score or -1.
ScipStore.prototype._fuzzyScore = function (query, candidate) {
  var qi = 0, score = 0, lastMatch = -1;
  for (var ci = 0; ci < candidate.length && qi < query.length; ci++) {
    if (candidate.charAt(ci) === query.charAt(qi)) {
      score += (lastMatch === ci - 1) ? 3 : 1;
      if (ci === 0 || candidate.charAt(ci - 1) === "." || candidate.charAt(ci - 1) === "_") score += 2;
      lastMatch = ci;
      qi++;
    }
  }
  return qi < query.length ? -1 : score;
};

// Search on display names with fuzzy fallback. Returns JSON array.
ScipStore.prototype.search = function (query) {
  if (!query || query.length < 1) return "[]";
  var q = query.toLowerCase();
  var scored = [];
  var syms = this._symbols;
  for (var sym in syms) {
    if (!syms.hasOwnProperty(sym)) continue;
    var entry = syms[sym];
    var name = (entry.name || "").toLowerCase();
    // Exact substring match (high priority)
    if (name.indexOf(q) !== -1) {
      scored.push({ s: 1000 + (name === q ? 500 : 0), sym: sym, entry: entry });
    } else {
      // Fuzzy match fallback
      var fs = this._fuzzyScore(q, name);
      if (fs > 0) scored.push({ s: fs, sym: sym, entry: entry });
    }
  }
  scored.sort(function (a, b) { return b.s - a.s; });
  var results = [];
  for (var i = 0; i < scored.length && results.length < 20; i++) {
    var e = scored[i];
    results.push({
      symbol: e.sym,
      display_name: e.entry.name,
      kind: e.entry.kind,
      doc_url: e.entry.doc_url || null,
    });
  }
  return JSON.stringify(results);
};

// Find all occurrences of a symbol. Returns JSON array.
ScipStore.prototype.findReferences = function (symbol) {
  var refs = [];
  var files = this._files;
  for (var file in files) {
    if (!files.hasOwnProperty(file)) continue;
    var occs = files[file];
    for (var i = 0; i < occs.length; i++) {
      if (occs[i].sym === symbol) {
        refs.push({
          file: file,
          line: occs[i].line,
          col_start: occs[i].cs,
          col_end: occs[i].ce,
          is_def: !!occs[i].def,
        });
      }
    }
  }
  return JSON.stringify(refs);
};

// Return the doc URL for a symbol, or null.
ScipStore.prototype.docUrl = function (symbol) {
  var info = this._symbols[symbol];
  return info ? (info.doc_url || null) : null;
};

// Return a CSS-safe class name for a SCIP symbol (e.g. "sym-a3f1b2").
ScipStore.prototype.symbolClass = function (symbol) {
  if (!this._classCache) this._classCache = {};
  if (this._classCache[symbol]) return this._classCache[symbol];
  // djb2 hash → 6-char hex
  var h = 5381;
  for (var i = 0; i < symbol.length; i++) {
    h = ((h << 5) + h + symbol.charCodeAt(i)) >>> 0;
  }
  var cls = "sym-" + ("000000" + h.toString(16)).slice(-6);
  this._classCache[symbol] = cls;
  return cls;
};

// Return just the 6-char hex hash for a symbol (without the "sym-" prefix).
// Used by feHighlight() which generates rules for sym-, sym-d-, sym-r- variants.
ScipStore.prototype.symbolHash = function (symbol) {
  return this.symbolClass(symbol).substring(4);
};

// Reverse lookup: find SCIP symbol string for a doc URL. Returns symbol or null.
ScipStore.prototype.symbolForDocUrl = function (docUrl) {
  // Lazily build reverse index on first call
  if (!this._byDocUrl) {
    this._byDocUrl = {};
    var syms = this._symbols;
    for (var sym in syms) {
      if (!syms.hasOwnProperty(sym)) continue;
      var url = syms[sym].doc_url;
      if (url) this._byDocUrl[url] = sym;
    }
  }
  return this._byDocUrl[docUrl] || null;
};
