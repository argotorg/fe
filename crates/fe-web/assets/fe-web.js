// fe-web.js — Static documentation renderer for Fe
// Ports doc-viewer SSR components to vanilla JS with hash-based routing.

(function () {
  "use strict";

  // ============================================================================
  // Data access
  // ============================================================================

  /** @returns {object} The DocIndex */
  function getIndex() {
    return window.FE_DOC_INDEX || { items: [], modules: [] };
  }

  /** @returns {object|null} The ScipStore WASM instance, if available */
  function getScipStore() {
    return window.FE_SCIP || null;
  }

  // ============================================================================
  // Routing (hash-based for file:// support)
  // ============================================================================

  function currentPath() {
    var h = location.hash.replace(/^#\/?/, "");
    // Strip in-page anchor (separated by ~)
    var tilde = h.indexOf("~");
    if (tilde !== -1) h = h.substring(0, tilde);
    return decodeURIComponent(h);
  }

  /** Extract the in-page anchor from the hash (after ~), or null. */
  function currentAnchor() {
    var h = location.hash.replace(/^#\/?/, "");
    var tilde = h.indexOf("~");
    if (tilde === -1) return null;
    return decodeURIComponent(h.substring(tilde + 1));
  }

  function navigate(path) {
    location.hash = "#" + path;
  }

  function itemHref(urlPath) {
    return "#" + urlPath;
  }

  function moduleHref(modulePath) {
    return "#" + modulePath + "/mod";
  }

  // ============================================================================
  // Utility
  // ============================================================================

  function esc(s) {
    var d = document.createElement("div");
    d.textContent = s;
    return d.innerHTML;
  }

  function kindBadge(kind) {
    return '<span class="kind-badge ' + esc(kind) + '">' + esc(kind) + "</span>";
  }

  /** Group items by kind, sorted by display order */
  function groupByKind(items, orderFn) {
    var groups = {};
    var order = {};
    items.forEach(function (item) {
      var k = orderFn(item);
      if (!groups[k.key]) {
        groups[k.key] = { kind: k.kind, plural: k.plural, items: [] };
        order[k.key] = k.order;
      }
      groups[k.key].items.push(item);
    });
    return Object.keys(groups)
      .sort(function (a, b) { return order[a] - order[b]; })
      .map(function (k) { return groups[k]; });
  }

  var ITEM_KIND_INFO = {
    module: { str: "mod", plural: "Modules", order: 0 },
    function: { str: "fn", plural: "Functions", order: 6 },
    struct: { str: "struct", plural: "Structs", order: 3 },
    enum: { str: "enum", plural: "Enums", order: 4 },
    trait: { str: "trait", plural: "Traits", order: 1 },
    contract: { str: "contract", plural: "Contracts", order: 2 },
    type_alias: { str: "type", plural: "Type Aliases", order: 5 },
    const: { str: "const", plural: "Constants", order: 7 },
    impl: { str: "impl", plural: "Implementations", order: 8 },
    impl_trait: { str: "impl", plural: "Trait Implementations", order: 9 },
  };

  function kindStr(kind) {
    return (ITEM_KIND_INFO[kind] || {}).str || kind;
  }

  function kindPlural(kind) {
    return (ITEM_KIND_INFO[kind] || {}).plural || kind;
  }

  function kindOrder(kind) {
    var info = ITEM_KIND_INFO[kind];
    return info ? info.order : 99;
  }

  function kindDisplayName(kind) {
    var names = {
      module: "Module", function: "Function", struct: "Struct",
      enum: "Enum", trait: "Trait", contract: "Contract",
      type_alias: "Type Alias", const: "Constant",
      impl: "Implementation", impl_trait: "Trait Implementation",
    };
    return names[kind] || kind;
  }

  var CHILD_KIND_INFO = {
    field: { plural: "Fields", anchor: "field", order: 1 },
    variant: { plural: "Variants", anchor: "variant", order: 0 },
    method: { plural: "Methods", anchor: "tymethod", order: 4 },
    assoc_type: { plural: "Associated Types", anchor: "associatedtype", order: 2 },
    assoc_const: { plural: "Associated Constants", anchor: "associatedconstant", order: 3 },
  };

  // ============================================================================
  // Rich Signature Rendering
  // ============================================================================

  function renderRichSignature(rich, fallback, highlightedFallback) {
    if (rich && rich.length > 0) {
      var jsonAttr = esc(JSON.stringify(rich));
      return "<fe-signature data='" + jsonAttr + "'>" + esc(fallback || "") + "</fe-signature>";
    }
    if (highlightedFallback) {
      return '<fe-code-block lang="fe" highlighted>' + highlightedFallback + "</fe-code-block>";
    }
    return '<fe-code-block lang="fe">' + esc(fallback || "") + "</fe-code-block>";
  }

  // ============================================================================
  // Sidebar Rendering
  // ============================================================================

  function renderSidebar(modules, curPath) {
    var html = '<nav class="doc-sidebar">';
    html += '<div class="sidebar-header">';
    html += '<h1><a href="#" onclick="return false;">Fe Docs</a></h1>';
    html += "<fe-search></fe-search>";
    html += "</div>";
    html += '<div class="sidebar-nav">';
    modules.forEach(function (mod) {
      html += renderModuleNav(mod, curPath);
    });
    html += "</div></nav>";
    return html;
  }

  function renderModuleNav(mod, curPath) {
    var modUrl = mod.path + "/mod";
    var isCurrent = modUrl === curPath;
    var isExpanded = curPath.indexOf(mod.path) === 0;
    var hasChildren = mod.children && mod.children.length > 0;
    var hasItems = mod.items && mod.items.length > 0;

    var html = '<details class="nav-module-tree"' + (isExpanded ? " open" : "") + ">";
    html += '<summary class="' + (isCurrent ? "nav-module current" : "nav-module") + '">';
    html += '<a href="' + moduleHref(mod.path) + '">' + esc(mod.name) + "</a>";
    html += "</summary>";
    html += '<div class="nav-module-content">';

    if (hasChildren) {
      html += '<div class="nav-submodules">';
      mod.children.forEach(function (child) {
        html += renderModuleNav(child, curPath);
      });
      html += "</div>";
    }

    if (hasItems) {
      // Group items by kind
      var grouped = groupByKind(mod.items, function (item) {
        return {
          key: item.kind,
          kind: kindStr(item.kind),
          plural: kindPlural(item.kind),
          order: kindOrder(item.kind),
        };
      });

      html += '<div class="nav-groups">';
      grouped.forEach(function (group) {
        html += '<div class="nav-kind-group">';
        html += '<h4 class="nav-kind-header">' + esc(group.plural) + "</h4>";
        html += '<ul class="nav-items">';
        group.items.forEach(function (item) {
          var itemUrl = item.path + "/" + kindStr(item.kind);
          var itemCurrent = itemUrl === curPath;
          html += '<li class="' + (itemCurrent ? "current" : "") + '">';
          html += '<a href="' + itemHref(itemUrl) + '">';
          html += kindBadge(kindStr(item.kind));
          html += " " + esc(item.name);
          html += "</a></li>";
        });
        html += "</ul></div>";
      });
      html += "</div>";
    }

    html += "</div></details>";
    return html;
  }

  // ============================================================================
  // Doc Item Rendering
  // ============================================================================

  function renderDocItem(item, index) {
    var isModule = item.kind === "module";
    var html = '<article class="doc-item">';

    // Breadcrumbs
    html += renderBreadcrumbs(item);

    // Header with kind badge and name
    html += '<div class="item-header"><div class="item-title">';
    html += '<span class="kind-badge ' + esc(kindStr(item.kind)) + '">' + esc(kindDisplayName(item.kind)) + "</span>";
    html += "<h1>" + esc(item.name) + "</h1>";
    html += "</div></div>";

    // Signature (non-modules only)
    if (!isModule && item.signature) {
      html += '<pre class="signature">';
      html += renderRichSignature(item.rich_signature, item.signature, item.highlighted_signature);
      html += "</pre>";
    }

    // Documentation body
    if (item.docs) {
      html += renderDocContent(item.docs);
    }

    // Module members (for module items)
    if (isModule) {
      var modContent = findModuleContent(index.modules, item.path);
      if (modContent) {
        html += renderModuleMembers(modContent.items, modContent.submodules);
      }
    }

    // Children (fields, variants, methods)
    if (item.children && item.children.length > 0) {
      html += renderChildren(item.children);
    }

    // Trait implementations
    if (item.trait_impls && item.trait_impls.length > 0) {
      html += renderTraitImpls(item.trait_impls);
    }

    // Implementors (for trait pages)
    if (item.implementors && item.implementors.length > 0) {
      html += renderImplementors(item.implementors);
    }

    html += "</article>";
    return html;
  }

  function renderBreadcrumbs(item) {
    var segments = item.path.split("::");
    var html = '<nav class="breadcrumb">';
    var accumulated = "";
    segments.forEach(function (seg, i) {
      if (i > 0) {
        accumulated += "::";
        html += '<span class="breadcrumb-sep">::</span>';
      }
      accumulated += seg;
      var isLast = i === segments.length - 1;
      if (isLast) {
        html += '<span class="breadcrumb-current">' + esc(seg) + "</span>";
      } else {
        html += '<a href="' + itemHref(accumulated + "/mod") + '" class="breadcrumb-link">' + esc(seg) + "</a>";
      }
    });
    html += "</nav>";
    return html;
  }

  function renderDocContent(docs) {
    // Use pre-rendered html_body if available, otherwise use raw body
    var bodyHtml = docs.html_body || esc(docs.body || "");
    return '<div class="docs">' + bodyHtml + "</div>";
  }

  // ============================================================================
  // Children (Fields, Variants, Methods)
  // ============================================================================

  function renderChildren(children) {
    var grouped = groupByKind(children, function (child) {
      var info = CHILD_KIND_INFO[child.kind] || { plural: child.kind, anchor: child.kind, order: 99 };
      return { key: child.kind, kind: child.kind, plural: info.plural, order: info.order };
    });

    var html = '<div class="children-sections">';
    grouped.forEach(function (group) {
      var info = CHILD_KIND_INFO[group.kind] || { anchor: group.kind };
      var sectionId = info.anchor + "s";
      html += '<section class="children-section" id="' + esc(sectionId) + '">';
      html += "<h2>" + esc(group.plural);
      html += '<a href="#' + esc(sectionId) + '" class="anchor">\u00a7</a>';
      html += "</h2>";
      html += '<div class="member-list">';
      group.items.forEach(function (child) {
        var anchorId = info.anchor + "." + child.name;
        html += '<div class="member-item" id="' + esc(anchorId) + '">';
        html += '<div class="member-header">';
        html += '<a href="#' + esc(anchorId) + '" class="anchor">\u00a7</a>';
        var sig = child.signature || child.name;
        html += renderRichSignature(child.rich_signature, sig, child.highlighted_signature);
        html += "</div>";
        if (child.docs) {
          html += '<div class="member-docs">' + esc(child.docs) + "</div>";
        }
        html += "</div>";
      });
      html += "</div></section>";
    });
    html += "</div>";
    return html;
  }

  // ============================================================================
  // Trait Implementations
  // ============================================================================

  function renderTraitImpls(impls) {
    var traitImpls = [];
    var inherentImpls = [];
    impls.forEach(function (impl) {
      if (impl.trait_name) {
        traitImpls.push(impl);
      } else {
        inherentImpls.push(impl);
      }
    });

    var html = '<div class="implementations">';

    if (inherentImpls.length > 0) {
      html += '<section class="inherent-impls" id="implementations">';
      html += '<h2>Implementations<a href="#implementations" class="anchor">\u00a7</a></h2>';
      html += '<div class="impl-list">';
      inherentImpls.forEach(function (impl, idx) {
        html += renderImplBlock(impl, "impl-" + idx);
      });
      html += "</div></section>";
    }

    if (traitImpls.length > 0) {
      html += '<section class="trait-impls" id="trait-implementations">';
      html += '<h2>Trait Implementations<a href="#trait-implementations" class="anchor">\u00a7</a></h2>';
      html += '<div class="impl-list">';
      traitImpls.forEach(function (impl) {
        var anchorId = "impl-" + impl.trait_name.replace(/[<> ,]/g, "_");
        html += renderImplBlock(impl, anchorId);
      });
      html += "</div></section>";
    }

    html += "</div>";
    return html;
  }

  function renderImplBlock(impl_, anchorId) {
    var isTraitImpl = !!impl_.trait_name;
    var headerDisplay = isTraitImpl ? "impl " + impl_.trait_name : impl_.signature;

    var html = '<details class="impl-block toggle" open id="' + esc(anchorId) + '">';
    html += "<summary>";
    html += '<span class="impl-header">';
    html += '<a href="#' + esc(anchorId) + '" class="anchor">\u00a7</a>';
    html += "<h3><code>" + esc(headerDisplay) + "</code></h3>";
    html += "</span></summary>";
    html += '<div class="impl-content">';

    // Signature for trait impls
    if (isTraitImpl) {
      html += '<pre class="rust impl-signature">';
      html += renderRichSignature(impl_.rich_signature, impl_.signature, impl_.highlighted_signature);
      html += "</pre>";
    }

    // Methods
    if (impl_.methods && impl_.methods.length > 0) {
      html += '<div class="impl-items">';
      impl_.methods.forEach(function (method) {
        var methodAnchor = "method." + method.name;
        html += renderMethodItem(method, methodAnchor);
      });
      html += "</div>";
    }

    html += "</div></details>";
    return html;
  }

  function renderMethodItem(method, anchorId) {
    var headerHtml =
      '<div class="method-header">' +
      '<a href="#' + esc(anchorId) + '" class="anchor">\u00a7</a>' +
      '<h4 class="code-header">' + renderRichSignature(method.rich_signature, method.signature, method.highlighted_signature) + "</h4>" +
      "</div>";

    if (method.docs) {
      return '<details class="method-item toggle" open id="' + esc(anchorId) + '">' +
        "<summary>" + headerHtml + "</summary>" +
        '<div class="method-docblock">' + esc(method.docs) + "</div>" +
        "</details>";
    }
    return '<div class="method-item no-toggle" id="' + esc(anchorId) + '">' + headerHtml + "</div>";
  }

  // ============================================================================
  // Module Members
  // ============================================================================

  function renderModuleMembers(items, submodules) {
    if ((!submodules || submodules.length === 0) && (!items || items.length === 0)) {
      return "";
    }

    var html = '<div class="module-items">';

    // Submodules first
    if (submodules && submodules.length > 0) {
      html += '<section class="item-table" id="modules">';
      html += "<h2>Modules</h2>";
      html += '<div class="item-list">';
      submodules.forEach(function (sub) {
        html += '<div class="item-row">';
        html += '<div class="item-name"><a href="' + moduleHref(sub.path) + '"><code>' + esc(sub.name) + "</code></a></div>";
        html += '<div class="item-summary"></div>';
        html += "</div>";
      });
      html += "</div></section>";
    }

    // Other items grouped by kind
    if (items && items.length > 0) {
      var grouped = groupByKind(items, function (item) {
        return {
          key: item.kind,
          kind: kindStr(item.kind),
          plural: kindPlural(item.kind),
          order: kindOrder(item.kind),
        };
      });

      grouped.forEach(function (group) {
        html += '<section class="item-table" id="' + esc(group.items[0] ? kindStr(group.items[0].kind) : "") + '">';
        html += "<h2>" + esc(group.plural) + "</h2>";
        html += '<div class="item-list">';
        group.items.forEach(function (item) {
          var url = item.path + "/" + kindStr(item.kind);
          html += '<div class="item-row">';
          html += '<div class="item-name"><a href="' + itemHref(url) + '"><code>' + esc(item.name) + "</code></a></div>";
          html += '<div class="item-summary">' + esc(item.summary || "") + "</div>";
          html += "</div>";
        });
        html += "</div></section>";
      });
    }

    html += "</div>";
    return html;
  }

  // ============================================================================
  // Implementors (for trait pages)
  // ============================================================================

  function renderImplementors(implementors) {
    var html = '<section class="implementors" id="implementors">';
    html += '<h2>Implementors<a href="#implementors" class="anchor">\u00a7</a></h2>';
    html += '<div class="implementor-list">';
    implementors.forEach(function (imp) {
      var anchorId = "impl-" + imp.type_name.replace(/[<> ,]/g, "_");
      var implLink = itemHref(imp.type_url + "~impl-" + imp.trait_name);
      html += '<div class="implementor-item" id="' + esc(anchorId) + '">';
      html += '<a href="#' + esc(anchorId) + '" class="anchor">\u00a7</a>';
      html += '<code class="implementor-sig">';
      html += renderRichSignature(imp.rich_signature, imp.signature, imp.highlighted_signature);
      html += "</code>";
      html += '<a href="' + implLink + '" class="impl-link" title="Go to implementation">\u2192</a>';
      html += "</div>";
    });
    html += "</div></section>";
    return html;
  }

  // ============================================================================
  // Module content lookup
  // ============================================================================

  function findModuleContent(modules, path) {
    for (var i = 0; i < modules.length; i++) {
      var mod = modules[i];
      if (mod.path === path) {
        var submodules = (mod.children || []).map(function (child) {
          return { name: child.name, path: child.path };
        });
        return { items: mod.items || [], submodules: submodules };
      }
      if (mod.children) {
        var found = findModuleContent(mod.children, path);
        if (found) return found;
      }
    }
    return null;
  }

  // ============================================================================
  // Main render / router
  // ============================================================================

  function render() {
    var index = getIndex();
    var path = currentPath();

    var sidebarEl = document.getElementById("sidebar");
    var contentEl = document.getElementById("content");
    if (!sidebarEl || !contentEl) return;

    // Render sidebar
    sidebarEl.innerHTML = renderSidebar(index.modules || [], path);

    // Find and render the requested item
    var item = findByUrl(index, path);

    if (item) {
      contentEl.innerHTML = renderDocItem(item, index);
    } else if (path === "" || path === "/") {
      contentEl.innerHTML =
        '<div class="not-found"><h1>Fe Documentation</h1>' +
        "<p>Select an item from the sidebar to view its documentation.</p></div>";
    } else {
      contentEl.innerHTML =
        '<div class="not-found"><h1>Item Not Found</h1>' +
        "<p>The documentation item <code>" + esc(path) + "</code> could not be found.</p>" +
        '<p class="not-found-hint">It may have been renamed or removed.</p></div>';
    }

    // Scroll to in-page anchor (e.g. ~impl-Bound)
    var anchor = currentAnchor();
    if (anchor) {
      var el = document.getElementById(anchor);
      if (el) el.scrollIntoView({ behavior: "smooth" });
    }
  }

  /** Find an item by URL path (mirrors DocIndex::find_by_url) */
  function findByUrl(index, urlPath) {
    if (!urlPath) return null;
    var items = index.items || [];

    // Try path/kind format
    var slashIdx = urlPath.lastIndexOf("/");
    if (slashIdx !== -1) {
      var path = urlPath.substring(0, slashIdx);
      var kindSuffix = urlPath.substring(slashIdx + 1);
      // Reverse-map kind suffix to serde name
      var kindMap = {
        mod: "module", fn: "function", struct: "struct", enum: "enum",
        trait: "trait", contract: "contract", type: "type_alias",
        const: "const", impl: "impl",
      };
      var kindName = kindMap[kindSuffix];
      if (kindName) {
        for (var i = 0; i < items.length; i++) {
          if (items[i].path === path && items[i].kind === kindName) {
            return items[i];
          }
        }
      }
    }

    // Fallback: find by path alone
    for (var j = 0; j < items.length; j++) {
      if (items[j].path === urlPath) {
        return items[j];
      }
    }
    return null;
  }

  // ============================================================================
  // LSP WebSocket Client (live mode)
  // ============================================================================

  /**
   * Connect to an LSP server over WebSocket for live features.
   *
   * Usage: window.FE_LSP = connectLsp("ws://127.0.0.1:9000");
   *
   * @param {string} wsUrl - WebSocket URL of the LSP server
   * @returns {object} LSP client handle with send/request methods
   */
  function connectLsp(wsUrl) {
    var ws = new WebSocket(wsUrl);
    var nextId = 1;
    var pending = {};  // id → {resolve, reject}
    var diagnostics = {};  // uri → Diagnostic[]
    var ready = false;

    ws.onopen = function () {
      // Send LSP initialize request
      sendRequest("initialize", {
        processId: null,
        capabilities: {
          textDocument: {
            publishDiagnostics: { relatedInformation: true }
          }
        },
        rootUri: null,
      }).then(function (result) {
        // Send initialized notification
        sendNotification("initialized", {});
        ready = true;
        console.log("[fe-lsp] Connected to LSP server:", result.serverInfo || {});
      });
    };

    ws.onmessage = function (event) {
      var msg;
      try { msg = JSON.parse(event.data); } catch (_) { return; }

      if (msg.id != null && pending[msg.id]) {
        // Response to a request
        if (msg.error) {
          pending[msg.id].reject(msg.error);
        } else {
          pending[msg.id].resolve(msg.result);
        }
        delete pending[msg.id];
      } else if (msg.method === "textDocument/publishDiagnostics") {
        // Notification: diagnostics update
        var params = msg.params || {};
        diagnostics[params.uri] = params.diagnostics || [];
        // Dispatch event for any listening components
        document.dispatchEvent(new CustomEvent("fe-diagnostics", {
          detail: { uri: params.uri, diagnostics: params.diagnostics || [] }
        }));
      }
    };

    ws.onerror = function (err) {
      console.warn("[fe-lsp] WebSocket error:", err);
    };

    ws.onclose = function () {
      ready = false;
      console.log("[fe-lsp] Disconnected from LSP server");
    };

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
      /** Send an LSP request and return a Promise for the result. */
      request: sendRequest,
      /** Send an LSP notification (no response expected). */
      notify: sendNotification,
      /** Get cached diagnostics for a URI. */
      getDiagnostics: function (uri) { return diagnostics[uri] || []; },
      /** Whether the LSP connection is ready (initialized). */
      isReady: function () { return ready; },
      /** Close the connection. */
      close: function () { ws.close(); },
    };
  }

  // Expose connectLsp globally for browser use
  window.connectLsp = connectLsp;

  // ============================================================================
  // Initialization
  // ============================================================================

  function init() {
    render();
    window.addEventListener("hashchange", render);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
