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

// ============================================================================
// Schema migration — normalizes old docs.json to the current format.
// When SCHEMA_VERSION is bumped in model.rs, add a migration case here
// and update FE_CURRENT_SCHEMA to match.
// ============================================================================
var FE_CURRENT_SCHEMA = %%SCHEMA_VERSION%%;

function feMigrate(data) {
  if (!data) return data;
  var v = (data.schema_version != null) ? data.schema_version : 0;

  // Bare DocIndex (no envelope) — wrap it
  if (v === 0 && !data.index && data.items) {
    data = { schema_version: 0, index: data, scip: null };
  }

  if (v > FE_CURRENT_SCHEMA) {
    console.warn("[fe-web] docs.json schema v" + v + " > viewer v" + FE_CURRENT_SCHEMA);
    return data;
  }
  if (v === FE_CURRENT_SCHEMA) return data;

  if (v < 1) {
    // v0 → v1: envelope normalization only, no field changes
    data.schema_version = 1;
  }

  if (v < 2) {
    // v1 → v2: added msg/msg_variant item kinds, no structural changes
    data.schema_version = 2;
  }

  if (v < 3) {
    // v2 → v3: msg variants are no longer top-level DocItems; they live as
    // `kind: "variant"` children of their parent msg DocItem (mirrors enum
    // variants). SCIP doc_url for msg variants moved from
    // `<path>/msg_variant` to `<parent>/msg~variant.<name>`.
    //
    // For v<3 data: drop stale top-level msg_variant items from index.items
    // and rewrite any lingering `/msg_variant` SCIP doc_urls to the anchor
    // form. Best-effort — consumers holding v<3 docs.json should regenerate.
    if (data.index && data.index.items) {
      data.index.items = data.index.items.filter(function (it) {
        return it && it.kind !== "msg_variant";
      });
    }
    if (data.scip && data.scip.symbols) {
      var syms = data.scip.symbols;
      for (var k in syms) {
        if (!syms.hasOwnProperty(k)) continue;
        var url = syms[k].doc_url;
        if (!url) continue;
        // /msg_variant → parent /msg~variant.<name>. Drop any legacy
        // sub-anchor (e.g. ~field.x) — the router splits on the first ~,
        // so a doubled tilde would produce a hash that matches no element
        // ID. Migrated deep-links land on the variant row instead.
        var m = url.match(/^(.*)::([^:]+)\/msg_variant(~.*)?$/);
        if (m) {
          syms[k].doc_url = m[1] + "/msg~variant." + m[2];
        }
      }
    }
    data.schema_version = 3;
  }

  return data;
}

// ============================================================================
// ScipStore
// ============================================================================

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

// ============================================================================
// Shared helpers (used by fe-code-block, fe-doc-item, fe-symbol-link, etc.)
// ============================================================================

/** Escape HTML special characters. */
function feEscapeHtml(s) {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}

/** Look up a DocIndex item by path. Returns the item or null. */
function feFindItem(path) {
  var index = window.FE_DOC_INDEX;
  if (!index || !index.items) return null;
  for (var i = 0; i < index.items.length; i++) {
    if (index.items[i].path === path) return index.items[i];
  }
  return null;
}

/**
 * Wait for FE_DOC_INDEX to be available, then call the callback.
 * Returns true if data is already available (callback called synchronously),
 * false if waiting (callback will be called later).
 *
 * Multiple calls coalesce on a single event listener to avoid redundant
 * re-renders when many components mount before data loads.
 */
var _feReadyCallbacks = null;
function feWhenReady(callback) {
  var index = window.FE_DOC_INDEX;
  if (index && index.items) {
    return true;
  }
  if (!_feReadyCallbacks) {
    _feReadyCallbacks = [];
    document.addEventListener("fe-web-ready", function onReady() {
      document.removeEventListener("fe-web-ready", onReady);
      var cbs = _feReadyCallbacks;
      _feReadyCallbacks = null;
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    });
  }
  _feReadyCallbacks.push(callback);
  return false;
}

/**
 * Enrich an anchor element with SCIP hover highlighting and tooltip.
 * `docUrl` is the doc path (e.g. "mylib::Foo/struct").
 */
function feEnrichLink(anchor, docUrl) {
  var scip = window.FE_SCIP;
  if (!scip) return;

  var symbol = scip.symbolForDocUrl(docUrl);

  // Fallback: name search
  if (!symbol) {
    var text = anchor.textContent.trim();
    if (text) {
      try {
        var results = JSON.parse(scip.search(text));
        for (var i = 0; i < results.length; i++) {
          if (results[i].display_name === text) {
            symbol = results[i].symbol;
            break;
          }
        }
      } catch (_) {}
    }
  }
  if (!symbol) return;

  anchor.classList.add(scip.symbolClass(symbol));

  var hash = scip.symbolHash(symbol);
  anchor.addEventListener("mouseenter", function () { feHighlight(hash); });
  anchor.addEventListener("mouseleave", feUnhighlight);

  var info = scip.symbolInfo(symbol);
  if (info) {
    try {
      var parsed = JSON.parse(info);
      if (parsed.documentation && parsed.documentation.length > 0) {
        anchor.title = parsed.documentation[0].replace(/```[\s\S]*?```/g, "").trim();
      }
    } catch (_) {}
  }
}

// ============================================================================
// Shared fetch cache for `src` attribute — multiple components sharing the
// same URL share a single fetch.  Returns a Promise that resolves to
// { index: DocIndex, scip: ScipStore|null }.
// ============================================================================
var _feSrcCache = {};

// Fetch JSON, trying .gz compressed version first.
// Falls back to uncompressed if .gz is not found or DecompressionStream is unavailable.
function feFetchJson(url) {
  var canDecompress = typeof DecompressionStream !== "undefined";
  var gzUrl = url + ".gz";

  var tryGz = canDecompress
    ? fetch(gzUrl).then(function (r) {
        if (!r.ok) return null; // fall back to uncompressed
        var ds = new DecompressionStream("gzip");
        var decompressed = r.body.pipeThrough(ds);
        return new Response(decompressed).json();
      }).catch(function () { return null; })
    : Promise.resolve(null);

  return tryGz.then(function (data) {
    if (data) return data;
    return fetch(url).then(function (r) {
      if (!r.ok) throw new Error("HTTP " + r.status + " loading " + url);
      return r.json();
    });
  });
}

function feLoadSrc(url) {
  if (_feSrcCache[url]) return _feSrcCache[url];
  _feSrcCache[url] = feFetchJson(url)
    .then(function (data) {
      // Migrate old docs.json formats to current schema
      data = feMigrate(data);

      var result = { index: null, scip: null };
      if (data && data.index) {
        result.index = data.index;
        if (data.scip) {
          result.scip = new ScipStore(data.scip);
        }
      } else if (data) {
        // Should not happen after migration, but fallback
        result.index = data;
      }
      // Also populate globals if not already set (first component to load wins)
      if (!window.FE_DOC_INDEX && result.index) {
        window.FE_DOC_INDEX = result.index;
      }
      if (!window.FE_SCIP && result.scip) {
        window.FE_SCIP = result.scip;
        document.dispatchEvent(new CustomEvent("fe-web-ready"));
      }
      return result;
    })
    .catch(function (err) {
      console.error("[fe-web] Failed to load", url, err);
      // Evict from cache so a retry can succeed
      delete _feSrcCache[url];
      return { index: null, scip: null };
    });
  return _feSrcCache[url];
}

// Explicit global exports — allows loading as type="module" without losing access
window.feHighlight = feHighlight;
window.feUnhighlight = feUnhighlight;
window.feSetDefaultHighlight = feSetDefaultHighlight;
window.feClearDefaultHighlight = feClearDefaultHighlight;
window.feEscapeHtml = feEscapeHtml;
window.feFindItem = feFindItem;
window.feWhenReady = feWhenReady;
window.feEnrichLink = feEnrichLink;
window.feLoadSrc = feLoadSrc;
window.feMigrate = feMigrate;

// ============================================================================
// LSP WebSocket Client (for `fe doc serve` live mode)
// ============================================================================

function feConnectLsp(wsUrl) {
  var ws = new WebSocket(wsUrl);
  var nextId = 1;
  var pending = {};
  var diagnostics = {};
  var ready = false;

  ws.onopen = function () {
    sendRequest("initialize", {
      processId: null,
      capabilities: { textDocument: { publishDiagnostics: { relatedInformation: true } } },
      rootUri: null,
    }).then(function (result) {
      sendNotification("initialized", {});
      ready = true;
      console.log("[fe-lsp] Connected:", result.serverInfo || {});
    });
  };

  ws.onmessage = function (event) {
    var msg;
    try { msg = JSON.parse(event.data); } catch (_) { return; }
    if (msg.id != null && pending[msg.id]) {
      if (msg.error) pending[msg.id].reject(msg.error);
      else pending[msg.id].resolve(msg.result);
      delete pending[msg.id];
    } else if (msg.method === "textDocument/publishDiagnostics") {
      var params = msg.params || {};
      diagnostics[params.uri] = params.diagnostics || [];
      document.dispatchEvent(new CustomEvent("fe-diagnostics", {
        detail: { uri: params.uri, diagnostics: params.diagnostics || [] }
      }));
    } else if (msg.method === "fe/docReload") {
      var p = msg.params || {};
      if (p.docIndex) window.FE_DOC_INDEX = p.docIndex;
      if (p.scipData) {
        var obj = typeof p.scipData === "string" ? JSON.parse(p.scipData) : p.scipData;
        window.FE_SCIP_DATA = obj;
        if (typeof ScipStore !== "undefined") window.FE_SCIP = new ScipStore(obj);
      }
      document.dispatchEvent(new CustomEvent("fe-web-ready"));
    } else if (msg.method === "fe/navigate") {
      var path = (msg.params || {}).path;
      if (path) document.dispatchEvent(new CustomEvent("fe-navigate", {
        bubbles: true, detail: { docPath: path }
      }));
    }
  };

  ws.onerror = function (err) { console.warn("[fe-lsp] Error:", err); };
  ws.onclose = function () { ready = false; console.log("[fe-lsp] Disconnected"); };

  function sendRequest(method, params) {
    return new Promise(function (resolve, reject) {
      var id = nextId++;
      pending[id] = { resolve: resolve, reject: reject };
      ws.send(JSON.stringify({ jsonrpc: "2.0", id: id, method: method, params: params }));
    });
  }
  function sendNotification(method, params) {
    ws.send(JSON.stringify({ jsonrpc: "2.0", method: method, params: params }));
  }

  return {
    request: sendRequest,
    notify: sendNotification,
    getDiagnostics: function (uri) { return diagnostics[uri] || []; },
    isReady: function () { return ready; },
    close: function () { ws.close(); },
  };
}
window.feConnectLsp = feConnectLsp;
