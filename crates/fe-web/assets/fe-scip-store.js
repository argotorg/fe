// ScipStore — Pure-JS symbol index built from pre-processed SCIP JSON.
//
// The server (Rust) converts the SCIP protobuf into a compact JSON object
// with two keys:
//   symbols: { [scip_symbol]: { name, kind, docs?, enclosing?, doc_url? } }
//   files:   { [path]: [ { line, cs, ce, sym, def? }, ... ] }
//
// Usage:
//   window.FE_SCIP = new ScipStore(window.FE_SCIP_DATA);

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
