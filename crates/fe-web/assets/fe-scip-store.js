// ScipStore — Pure-JS symbol index built from pre-processed SCIP JSON.
//
// The server (Rust) converts the SCIP protobuf into a compact JSON object
// with two keys:
//   symbols: { [scip_symbol]: { name, kind, docs?, enclosing?, doc_url? } }
//   files:   { [path]: [ { line, cs, ce, sym, def? }, ... ] }
//
// Usage:
//   window.FE_SCIP = new ScipStore(window.FE_SCIP_DATA);

// Shared <style> element for symbol hover highlighting.
// Inserting/removing CSS rules is all that's needed — the browser
// applies them to every element carrying the symbol's classes.
var _highlightSheet = null;

// Role-aware highlight: injects rules for closure (all occurrences),
// definition sites, and reference sites with different visual treatments.
function feHighlight(symHash) {
  if (!_highlightSheet) {
    _highlightSheet = document.createElement("style");
    _highlightSheet.id = "fe-sym-highlight";
    document.head.appendChild(_highlightSheet);
  }
  _highlightSheet.textContent =
    ".sym-" + symHash + " { background: rgba(74,222,128,0.12); border-radius: 2px; }" +
    ".sym-d-" + symHash + " { background: rgba(74,222,128,0.22); text-decoration: underline;" +
    " text-decoration-color: rgba(74,222,128,0.5); text-underline-offset: 2px; }";
}
function feUnhighlight() {
  if (_highlightSheet) _highlightSheet.textContent = "";
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

// Substring search on display names. Returns JSON array.
ScipStore.prototype.search = function (query) {
  if (!query || query.length < 1) return "[]";
  var q = query.toLowerCase();
  var results = [];
  var syms = this._symbols;
  for (var sym in syms) {
    if (!syms.hasOwnProperty(sym)) continue;
    var entry = syms[sym];
    var name = (entry.name || "").toLowerCase();
    if (name.indexOf(q) !== -1) {
      results.push({
        symbol: sym,
        display_name: entry.name,
        kind: entry.kind,
        doc_url: entry.doc_url || null,
      });
      if (results.length >= 20) break;
    }
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
