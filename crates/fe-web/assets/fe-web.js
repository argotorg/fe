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

  /** Detect compiler-generated types (tuples, etc.) that have no doc page. */
  function isGeneratedType(urlPath) {
    var slashIdx = urlPath.lastIndexOf("/");
    var path = slashIdx !== -1 ? urlPath.substring(0, slashIdx) : urlPath;
    // Tuple types: (T0, T1, ...) or (T0, T1, ...)/struct
    if (path.charAt(0) === "(") return true;
    return false;
  }

  function renderGeneratedType(urlPath) {
    var slashIdx = urlPath.lastIndexOf("/");
    var path = slashIdx !== -1 ? urlPath.substring(0, slashIdx) : urlPath;
    var html = '<div class="generated-type"><h1>Compiler-Generated Type</h1>';
    html += '<pre class="signature"><fe-code-block lang="fe">' + esc(path) + "</fe-code-block></pre>";
    html += "<p>This is a compiler-generated type. Tuple types like <code>" + esc(path) + "</code> ";
    html += "are created automatically by the compiler and do not have dedicated documentation pages.</p>";
    html += "</div>";
    return html;
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

  function renderRichSignature(rich, fallback, highlightedFallback, sigScope) {
    // Always emit raw text — client-side FeHighlighter handles
    // syntax highlighting and type linking via tree-sitter WASM + ScipStore.
    var attrs = 'lang="fe"';
    if (sigScope) attrs += ' data-scope="' + esc(sigScope) + '"';
    return "<fe-code-block " + attrs + ">" + esc(fallback || "") + "</fe-code-block>";
  }

  // ============================================================================
  // Sidebar Rendering
  // ============================================================================

  function renderSidebar(modules, curPath, builtinModules) {
    var html = '<nav class="doc-sidebar">';
    html += '<div class="sidebar-header">';
    html += '<h1><a href="#">Fe Docs</a></h1>';
    html += "<fe-search></fe-search>";
    html += "</div>";
    html += '<div class="sidebar-nav">';
    modules.forEach(function (mod) {
      html += renderModuleNav(mod, curPath);
    });
    if (builtinModules && builtinModules.length) {
      html += '<hr class="sidebar-separator">';
      builtinModules.forEach(function (mod) {
        html += renderModuleNav(mod, curPath);
      });
    }
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
    var parentUrl = item.path + "/" + kindStr(item.kind);
    var html = '<article class="doc-item">';

    // Breadcrumbs
    html += renderBreadcrumbs(item);

    // Header with kind badge, name, and source link
    html += '<div class="item-header"><div class="item-title">';
    html += '<span class="kind-badge ' + esc(kindStr(item.kind)) + '">' + esc(kindDisplayName(item.kind)) + "</span>";
    html += "<h1>" + esc(item.name) + "</h1>";
    html += "</div>";
    if (item.source && item.source.display_file) {
      var srcBase = window.FE_SOURCE_BASE;
      var srcHref = srcBase
        ? srcBase + "/" + item.source.display_file + (item.source.line ? "#L" + item.source.line : "")
        : "#";
      var srcTarget = srcBase ? ' target="_blank" rel="noopener"' : "";
      html += '<a class="src-link" href="' + esc(srcHref) + '"' + srcTarget + ">" + esc(item.source.display_file);
      if (item.source.line) html += ":" + item.source.line;
      html += "</a>";
    }
    html += "</div>";

    // Signature (non-modules only)
    if (!isModule && item.signature) {
      html += '<pre class="signature">';
      html += renderRichSignature(item.rich_signature, item.signature, item.highlighted_signature,
        item.sig_scope);
      html += "</pre>";
    }

    // Documentation body
    if (item.docs) {
      html += renderDocContent(item.docs);
    }

    // Module members (for module items)
    if (isModule) {
      var modContent = findModuleContent(index.modules, item.path)
        || findModuleContent(index.builtin_modules || [], item.path);
      if (modContent) {
        html += renderModuleMembers(modContent.items, modContent.submodules, index);
      }
    }

    // Children (fields, variants, methods)
    if (item.children && item.children.length > 0) {
      html += renderChildren(item.children, parentUrl);
    }

    // Trait implementations
    if (item.trait_impls && item.trait_impls.length > 0) {
      html += renderTraitImpls(item.trait_impls, parentUrl);
    }

    // Implementors (for trait pages)
    if (item.implementors && item.implementors.length > 0) {
      html += renderImplementors(item.implementors, parentUrl);
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
    var html = '<div class="docs">';
    // Use pre-rendered html_body if available, otherwise use raw body
    var bodyHtml = docs.html_body || esc(docs.body || "");
    html += bodyHtml;

    // Render doc sections as distinct visual blocks
    if (docs.sections && docs.sections.length > 0) {
      docs.sections.forEach(function (section) {
        var sectionId = "section-" + section.name.toLowerCase().replace(/\s+/g, "-");
        html += '<div class="doc-section" id="' + esc(sectionId) + '">';
        html += '<div class="doc-section-badge">' + esc(section.name) + "</div>";
        var sectionHtml = section.html_content || esc(section.content || "");
        html += '<div class="doc-section-content">' + sectionHtml + "</div>";
        html += "</div>";
      });
    }

    html += "</div>";
    return html;
  }

  // ============================================================================
  // Children (Fields, Variants, Methods)
  // ============================================================================

  function renderChildren(children, parentUrl) {
    var grouped = groupByKind(children, function (child) {
      var info = CHILD_KIND_INFO[child.kind] || { plural: child.kind, anchor: child.kind, order: 99 };
      return { key: child.kind, kind: child.kind, plural: info.plural, order: info.order };
    });

    var html = '<div class="children-sections">';
    grouped.forEach(function (group) {
      var info = CHILD_KIND_INFO[group.kind] || { anchor: group.kind };
      var sectionId = info.anchor + "s";
      html += '<section class="children-section">';
      html += '<h2 id="' + esc(sectionId) + '">' + esc(group.plural);
      html += '<a href="#' + esc(parentUrl) + "~" + esc(sectionId) + '" class="anchor">\u00a7</a>';
      html += "</h2>";
      html += '<div class="member-list">';
      group.items.forEach(function (child) {
        var anchorId = info.anchor + "." + child.name;
        html += '<div class="member-item" id="' + esc(anchorId) + '">';
        html += '<div class="member-header">';
        html += '<a href="#' + esc(parentUrl) + "~" + esc(anchorId) + '" class="anchor">\u00a7</a>';
        var sig = child.signature || child.name;
        html += renderRichSignature(child.rich_signature, sig, child.highlighted_signature, child.sig_scope);
        html += "</div>";
        if (child.docs) {
          var childDocsHtml = child.docs.html_body || esc(child.docs.body || child.docs.summary || "");
          html += '<div class="member-docs">' + childDocsHtml + "</div>";
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

  function renderTraitImpls(impls, parentUrl) {
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
      html += '<section class="inherent-impls">';
      html += '<h2 id="implementations">Implementations<a href="#' + esc(parentUrl) + '~implementations" class="anchor">\u00a7</a></h2>';
      html += '<div class="impl-list">';
      inherentImpls.forEach(function (impl, idx) {
        html += renderImplBlock(impl, "impl-" + idx, parentUrl);
      });
      html += "</div></section>";
    }

    if (traitImpls.length > 0) {
      html += '<section class="trait-impls">';
      html += '<h2 id="trait-implementations">Trait Implementations<a href="#' + esc(parentUrl) + '~trait-implementations" class="anchor">\u00a7</a></h2>';
      html += '<div class="impl-list">';
      traitImpls.forEach(function (impl) {
        var anchorId = "impl-" + impl.trait_name.replace(/[<> ,]/g, "_");
        html += renderImplBlock(impl, anchorId, parentUrl);
      });
      html += "</div></section>";
    }

    html += "</div>";
    return html;
  }

  function renderImplBlock(impl_, anchorId, parentUrl) {
    var isTraitImpl = !!impl_.trait_name;
    var headerDisplay = isTraitImpl ? "impl " + impl_.trait_name : impl_.signature;

    var html = '<details class="impl-block toggle" open id="' + esc(anchorId) + '">';
    html += "<summary>";
    html += '<span class="impl-header">';
    html += '<a href="#' + esc(parentUrl) + "~" + esc(anchorId) + '" class="anchor">\u00a7</a>';
    html += "<h3><code>" + esc(headerDisplay) + "</code></h3>";
    html += "</span></summary>";
    html += '<div class="impl-content">';

    // Signature for trait impls
    if (isTraitImpl) {
      html += '<pre class="rust impl-signature">';
      html += renderRichSignature(impl_.rich_signature, impl_.signature, impl_.highlighted_signature, impl_.sig_scope);
      html += "</pre>";
    }

    // Methods
    if (impl_.methods && impl_.methods.length > 0) {
      html += '<div class="impl-items">';
      impl_.methods.forEach(function (method) {
        var methodAnchor = "method." + method.name;
        html += renderMethodItem(method, methodAnchor, parentUrl, anchorId);
      });
      html += "</div>";
    }

    html += "</div></details>";
    return html;
  }

  function renderMethodItem(method, anchorId, parentUrl, implAnchor) {
    var anchorHref = parentUrl ? "#" + esc(parentUrl) + "~" + esc(anchorId) : "#" + esc(anchorId);
    var headerHtml =
      '<div class="method-header">' +
      '<a href="' + anchorHref + '" class="anchor">\u00a7</a>' +
      '<h4 class="code-header">' + renderRichSignature(method.rich_signature, method.signature, method.highlighted_signature, method.sig_scope) + "</h4>" +
      "</div>";

    if (method.docs) {
      var methodDocsHtml = method.docs.html_body || esc(method.docs.body || method.docs.summary || "");
      return '<details class="method-item toggle" open id="' + esc(anchorId) + '">' +
        "<summary>" + headerHtml + "</summary>" +
        '<div class="method-docblock">' + methodDocsHtml + "</div>" +
        "</details>";
    }
    return '<div class="method-item no-toggle" id="' + esc(anchorId) + '">' + headerHtml + "</div>";
  }

  // ============================================================================
  // Module Members
  // ============================================================================

  /** Look up a module's doc summary from the index items list. */
  function getModuleSummary(index, modulePath) {
    var items = index.items || [];
    for (var i = 0; i < items.length; i++) {
      if (items[i].path === modulePath && items[i].kind === "module") {
        var docs = items[i].docs;
        if (docs && docs.summary) return docs.summary;
        break;
      }
    }
    return "";
  }

  function renderModuleMembers(items, submodules, index) {
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
        var summary = index ? getModuleSummary(index, sub.path) : "";
        html += '<div class="item-row">';
        html += '<div class="item-name">' + kindBadge("mod") + '<a href="' + moduleHref(sub.path) + '"><code>' + esc(sub.name) + "</code></a></div>";
        html += '<div class="item-summary">' + esc(summary) + "</div>";
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
          html += '<div class="item-name">' + kindBadge(kindStr(item.kind)) + '<a href="' + itemHref(url) + '"><code>' + esc(item.name) + "</code></a></div>";
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

  function renderImplementors(implementors, parentUrl) {
    var html = '<section class="implementors">';
    html += '<h2 id="implementors">Implementors<a href="#' + esc(parentUrl) + '~implementors" class="anchor">\u00a7</a></h2>';
    html += '<div class="implementor-list">';
    implementors.forEach(function (imp) {
      var anchorId = "impl-" + imp.type_name.replace(/[<> ,]/g, "_");
      var implLink = itemHref(imp.type_url + "~impl-" + imp.trait_name);
      html += '<div class="implementor-item" id="' + esc(anchorId) + '">';
      html += '<a href="#' + esc(parentUrl) + "~" + esc(anchorId) + '" class="anchor">\u00a7</a>';
      html += '<code class="implementor-sig">';
      html += renderRichSignature(imp.rich_signature, imp.signature, imp.highlighted_signature, imp.sig_scope);
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

  /** Activate ambient SCIP highlighting for the current doc item or anchor. */
  function applyDefaultHighlight(path, anchor) {
    var scip = getScipStore();
    if (!scip || !path) {
      if (typeof feClearDefaultHighlight === "function") feClearDefaultHighlight();
      return;
    }

    // If we have an anchor, try to find a symbol for the full URL with anchor
    if (anchor) {
      var fullUrl = path + "~" + anchor;
      var anchorSym = scip.symbolForDocUrl(fullUrl);
      if (anchorSym) {
        feSetDefaultHighlight(scip.symbolHash(anchorSym));
        return;
      }
    }

    // Fall back to the base item's symbol
    var sym = scip.symbolForDocUrl(path);
    if (sym) {
      feSetDefaultHighlight(scip.symbolHash(sym));
    } else {
      if (typeof feClearDefaultHighlight === "function") feClearDefaultHighlight();
    }
  }

  /** Highlight the navigated-to anchor element with a CSS class (since :target
   *  doesn't work with our SPA hash routing). */
  function highlightAnchorTarget(contentEl, anchor) {
    // Clear any previous target highlight
    var prev = contentEl.querySelectorAll(".is-target");
    for (var i = 0; i < prev.length; i++) prev[i].classList.remove("is-target");

    if (anchor) {
      var el = document.getElementById(anchor);
      if (el) el.classList.add("is-target");
    }
  }

  var _lastRenderedPath = null;

  function render() {
    var index = getIndex();
    var path = currentPath();
    var anchor = currentAnchor();

    var sidebarEl = document.getElementById("sidebar");
    var contentEl = document.getElementById("content");
    if (!sidebarEl || !contentEl) return;

    // If only the anchor changed (same base path), skip full re-render —
    // just scroll to the anchor and update highlighting.
    if (path === _lastRenderedPath && anchor) {
      applyDefaultHighlight(path, anchor);
      highlightAnchorTarget(contentEl, anchor);
      var el = document.getElementById(anchor);
      if (el) el.scrollIntoView({ behavior: "smooth" });
      return;
    }
    _lastRenderedPath = path;

    // Render sidebar
    sidebarEl.innerHTML = renderSidebar(index.modules || [], path, index.builtin_modules || []);

    // Find and render the requested item
    var item = findByUrl(index, path);

    if (item) {
      contentEl.innerHTML = renderDocItem(item, index);
    } else if (path === "" || path === "/") {
      contentEl.innerHTML =
        '<div class="not-found"><h1>Fe Documentation</h1>' +
        "<p>Select an item from the sidebar to view its documentation.</p></div>";
    } else if (isGeneratedType(path)) {
      contentEl.innerHTML = renderGeneratedType(path);
    } else {
      contentEl.innerHTML =
        '<div class="not-found"><h1>Item Not Found</h1>' +
        "<p>The documentation item <code>" + esc(path) + "</code> could not be found.</p>" +
        '<p class="not-found-hint">It may have been renamed or removed.</p></div>';
    }

    // Build in-page section outline in sidebar
    buildPageOutline(contentEl, sidebarEl);

    // Default SCIP highlight: when viewing a specific item, highlight all
    // occurrences of that item's symbol across code blocks on the page.
    applyDefaultHighlight(path, anchor);

    // Scroll to in-page anchor (e.g. ~impl-Bound), or top for new pages
    if (anchor) {
      highlightAnchorTarget(contentEl, anchor);
      var el = document.getElementById(anchor);
      if (el) el.scrollIntoView({ behavior: "smooth" });
    } else {
      contentEl.scrollTop = 0;
      window.scrollTo(0, 0);
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
      } else if (msg.method === "fe/navigate") {
        // Editor hover/goto → navigate doc browser to the target item
        var p = (msg.params || {}).path;
        if (p) navigate(p);
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
  // In-Page Section Outline
  // ============================================================================

  var _outlineObserver = null;

  function buildPageOutline(contentEl, sidebarEl) {
    // Remove previous outline
    var prev = sidebarEl.querySelector(".page-outline");
    if (prev) prev.remove();
    if (_outlineObserver) { _outlineObserver.disconnect(); _outlineObserver = null; }

    // Collect section headings (h2), impl blocks, and method items.
    // Skip section[id] — its h2 child already represents it.
    var targets = contentEl.querySelectorAll("h2[id], details.impl-block[id], details.method-item[id], div.method-item[id]");
    if (targets.length === 0) return;

    var entries = [];
    for (var i = 0; i < targets.length; i++) {
      var el = targets[i];
      var id = el.id;
      var text = "";
      if (el.tagName === "H2") {
        text = el.textContent.replace("\u00a7", "").trim();
      } else if (id.indexOf("method.") === 0) {
        text = id.substring(7) + "()";
      } else if (el.classList.contains("impl-block")) {
        // Use the h3 text from the impl summary (e.g. "impl Display")
        var h3 = el.querySelector("summary h3");
        text = h3 ? h3.textContent.trim() : id;
        if (text.length > 50) text = text.substring(0, 47) + "\u2026";
      } else {
        var heading = el.querySelector("summary h3, summary h4");
        text = heading ? heading.textContent.trim()
          : (el.querySelector("summary") || el).textContent
              .replace("\u00a7", "").replace(/\u25b6/g, "").trim();
        if (text.length > 50) text = text.substring(0, 47) + "\u2026";
      }
      if (id && text) {
        entries.push({ id: id, text: text });
      }
    }
    if (entries.length === 0) return;

    var outline = document.createElement("div");
    outline.className = "page-outline";

    var header = document.createElement("h4");
    header.className = "outline-header";
    header.textContent = "On this page";
    outline.appendChild(header);

    var list = document.createElement("ul");
    list.className = "outline-list";

    var path = currentPath();
    entries.forEach(function (entry) {
      var li = document.createElement("li");
      var a = document.createElement("a");
      a.href = "#" + path + "~" + entry.id;
      a.textContent = entry.text;
      a.dataset.outlineId = entry.id;
      li.appendChild(a);
      list.appendChild(li);
    });

    outline.appendChild(list);

    // Insert after sidebar-nav
    var nav = sidebarEl.querySelector(".sidebar-nav");
    if (nav) {
      nav.parentNode.insertBefore(outline, nav);
    } else {
      sidebarEl.appendChild(outline);
    }

    // Highlight current section via IntersectionObserver.
    // Track all currently-visible sections and activate the topmost one,
    // so that clicking an outline link (which scrolls) doesn't get
    // hijacked by a different section passing through the viewport.
    if (typeof IntersectionObserver !== "undefined") {
      var links = list.querySelectorAll("a");
      var visibleIds = {};

      function updateActiveLink() {
        // Find the topmost visible section by comparing DOM order
        var bestId = null;
        for (var ti = 0; ti < targets.length; ti++) {
          if (visibleIds[targets[ti].id]) { bestId = targets[ti].id; break; }
        }
        for (var j = 0; j < links.length; j++) {
          links[j].classList.toggle("active", bestId !== null && links[j].dataset.outlineId === bestId);
        }
      }

      _outlineObserver = new IntersectionObserver(function (obs) {
        for (var oi = 0; oi < obs.length; oi++) {
          if (obs[oi].isIntersecting) {
            visibleIds[obs[oi].target.id] = true;
          } else {
            delete visibleIds[obs[oi].target.id];
          }
        }
        updateActiveLink();
      }, { rootMargin: "-80px 0px -70% 0px" });

      for (var k = 0; k < targets.length; k++) {
        _outlineObserver.observe(targets[k]);
      }
    }
  }

  // ============================================================================
  // Mobile Hamburger Menu
  // ============================================================================

  function initMobileMenu() {
    var btn = document.createElement("button");
    btn.className = "mobile-menu-btn";
    btn.textContent = "\u2630";
    btn.setAttribute("aria-label", "Toggle navigation");
    document.body.appendChild(btn);

    var backdrop = document.createElement("div");
    backdrop.className = "sidebar-backdrop";
    document.body.appendChild(backdrop);

    function closeSidebar() {
      var sidebar = document.querySelector(".doc-sidebar");
      if (sidebar) sidebar.classList.remove("open");
      backdrop.classList.remove("open");
    }

    btn.addEventListener("click", function () {
      var sidebar = document.querySelector(".doc-sidebar");
      if (sidebar) {
        var isOpen = sidebar.classList.toggle("open");
        backdrop.classList.toggle("open", isOpen);
      }
    });

    backdrop.addEventListener("click", closeSidebar);

    // Close sidebar on navigation
    window.addEventListener("hashchange", closeSidebar);
  }

  // ============================================================================
  // Initialization
  // ============================================================================

  function init() {
    initMobileMenu();
    render();
    window.addEventListener("hashchange", render);

    // Re-apply default highlight after tree-sitter WASM loads and code blocks
    // get SCIP-annotated (sym-HASH classes added). On initial load, render()
    // runs before the highlighter is ready, so the querySelectorAll in
    // _setHighlightStyles finds no matching elements.
    document.addEventListener("fe-highlighter-ready", function () {
      // Small delay to let code blocks re-render and run SCIP annotation
      setTimeout(function () { applyDefaultHighlight(currentPath(), currentAnchor()); }, 50);
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
