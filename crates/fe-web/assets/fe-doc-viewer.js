// <fe-doc-viewer> — Composable documentation viewer for Fe.
//
// Composes <fe-doc-nav> + content rendering + routing into a single
// drop-in component.  The host page can use this for a full-featured
// doc browser, or use the sub-components individually for custom layouts.
//
// Usage:
//   <fe-doc-viewer src="/docs.json" title="Fe Std Library"
//     back-href="/" back-label="Back to Guide" />
//
// Attributes:
//   src          — URL to docs.json (required)
//   title        — header title text
//   back-href    — URL for the back/home link
//   back-label   — text for the back/home link
//   routing      — "hash" (default), "path", or "none"
//   base         — base URL for path-based routing
//   filter       — passed to <fe-doc-nav> and content
//   filter-kind  — passed to <fe-doc-nav>
//   exclude      — passed to <fe-doc-nav> and content

class FeDocViewer extends HTMLElement {
  static get observedAttributes() {
    return ["src", "title", "routing", "filter", "filter-kind", "exclude", "lsp"];
  }

  attributeChangedCallback(name, oldVal, newVal) {
    if (oldVal === newVal) return;
    if (name === "src") { this._loadSrc(); return; }
    this._render();
  }

  connectedCallback() {
    this._createStructure();
    this._loadSrc();
    this._setupRouting();
    this._connectLsp();
  }

  disconnectedCallback() {
    if (this._hashHandler) {
      window.removeEventListener("hashchange", this._hashHandler);
      this._hashHandler = null;
    }
    if (this._targetStyle && this._targetStyle.parentNode) {
      this._targetStyle.parentNode.removeChild(this._targetStyle);
      this._targetStyle = null;
    }
    if (this._lsp) {
      this._lsp.close();
      this._lsp = null;
    }
  }

  // ---- LSP Live Mode ----

  _connectLsp() {
    var lspUrl = this.getAttribute("lsp");
    if (!lspUrl || typeof feConnectLsp !== "function") return;
    var self = this;
    this._lsp = feConnectLsp(lspUrl);

    // Listen for doc reload events (hot reload on save)
    document.addEventListener("fe-web-ready", function () {
      self._index = window.FE_DOC_INDEX;
      self._scip = window.FE_SCIP || null;
      self._currentPath = null;
      var raw = location.hash.replace(/^#\/?/, "");
      if (raw) {
        var parts = self._splitHash(raw);
        self._showItem(parts.path);
      } else {
        self._showWelcome();
      }
    });
  }

  _createStructure() {
    this.innerHTML = "";
    this.classList.add("fe-doc-viewer");

    // Header
    var header = document.createElement("div");
    header.className = "fe-doc-viewer-header";
    this._headerEl = header;
    this.appendChild(header);

    // Layout container
    var layout = document.createElement("div");
    layout.className = "fe-doc-viewer-layout";

    // Nav wrapper (for sidebar: nav + outline)
    var sidebar = document.createElement("div");
    sidebar.className = "fe-doc-viewer-sidebar";

    var nav = document.createElement("fe-doc-nav");
    nav.setAttribute("show-search", "");
    // Exclude landing-page example from API reference nav
    var existingExclude = this.getAttribute("exclude") || "";
    nav.setAttribute("exclude", existingExclude ? existingExclude + ",landing-page" : "landing-page");
    this._navEl = nav;
    sidebar.appendChild(nav);

    // Outline container (populated after content renders)
    var outline = document.createElement("div");
    outline.className = "page-outline";
    this._outlineEl = outline;
    sidebar.appendChild(outline);

    layout.appendChild(sidebar);

    // Content
    var content = document.createElement("div");
    content.className = "fe-doc-viewer-content";
    this._contentEl = content;
    layout.appendChild(content);

    this.appendChild(layout);

    // Mobile menu
    this._initMobileMenu();

    // Dynamic style element for anchor highlighting
    this._targetStyle = document.createElement("style");
    document.head.appendChild(this._targetStyle);

    // Listen for navigation events from nav and content
    var self = this;
    this.addEventListener("fe-navigate", function (e) {
      var docPath = e.detail.docPath;
      if (!docPath) return;

      // Prevent the code block from doing its own location.href navigation
      e.preventDefault();
      e.stopPropagation();

      var routing = self.getAttribute("routing") || "hash";
      if (routing === "hash") {
        var parts = self._splitHash(docPath);
        if (parts.path === self._currentPath) {
          // Same page — scroll to top or anchor
          self._contentEl.scrollTop = 0;
          if (parts.anchor) {
            self._scrollToAnchor(parts.anchor);
          } else {
            self._targetStyle.textContent = "";
          }
          // Update hash without triggering full re-render
          history.replaceState(null, "", "#" + docPath);
        } else {
          location.hash = "#" + docPath;
        }
      } else if (routing === "path") {
        var base = self.getAttribute("base") || "/";
        history.pushState(null, "", base + docPath);
        self._showItem(docPath);
      } else {
        self._showItem(docPath);
      }
    });
  }

  // ---- Mobile Menu ----

  _initMobileMenu() {
    var btn = document.createElement("button");
    btn.className = "mobile-menu-btn";
    btn.textContent = "\u2630";
    btn.setAttribute("aria-label", "Toggle navigation");
    this.appendChild(btn);

    var backdrop = document.createElement("div");
    backdrop.className = "sidebar-backdrop";
    this.appendChild(backdrop);

    var self = this;
    function closeSidebar() {
      var sidebar = self.querySelector(".fe-doc-viewer-sidebar");
      if (sidebar) sidebar.classList.remove("open");
      backdrop.classList.remove("open");
    }

    btn.addEventListener("click", function () {
      var sidebar = self.querySelector(".fe-doc-viewer-sidebar");
      if (sidebar) {
        var isOpen = sidebar.classList.toggle("open");
        backdrop.classList.toggle("open", isOpen);
      }
    });

    backdrop.addEventListener("click", closeSidebar);
    window.addEventListener("hashchange", closeSidebar);
    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") closeSidebar();
    });
  }

  // ---- Data Loading ----

  _loadSrc() {
    var src = this.getAttribute("src");
    var self = this;

    if (src) {
      if (this._navEl) this._navEl.setAttribute("src", src);

      feLoadSrc(src).then(function (result) {
        self._index = result.index;
        self._scip = result.scip;
        self._onDataReady();
      });
    } else {
      // No src attribute — use globals (self-contained static site mode)
      var index = window.FE_DOC_INDEX;
      if (index) {
        this._index = index;
        this._scip = window.FE_SCIP || null;
        this._onDataReady();
      } else {
        // Wait for globals to be populated
        var onReady = function () {
          document.removeEventListener("fe-web-ready", onReady);
          self._index = window.FE_DOC_INDEX;
          self._scip = window.FE_SCIP || null;
          self._onDataReady();
        };
        document.addEventListener("fe-web-ready", onReady);
      }
    }
  }

  _onDataReady() {
    this._render();

    var routing = this.getAttribute("routing") || "hash";
    if (routing === "hash") {
      var raw = location.hash.replace(/^#\/?/, "");
      if (raw) {
        var parts = this._splitHash(raw);
        this._currentPath = null;
        this._showItem(parts.path);
        if (parts.anchor) this._scrollToAnchor(parts.anchor);
      } else {
        this._showWelcome();
      }
    } else if (routing === "path") {
      var base = this.getAttribute("base") || "/";
      var pathname = location.pathname;
      if (pathname.indexOf(base) === 0) {
        var docPath = pathname.substring(base.length);
        if (docPath) {
          this._showItem(docPath);
        } else {
          this._showWelcome();
        }
      } else {
        this._showWelcome();
      }
    } else {
      this._showWelcome();
    }
  }

  // ---- Routing ----

  _setupRouting() {
    var routing = this.getAttribute("routing") || "hash";
    if (routing !== "hash") return;

    var self = this;
    this._hashHandler = function () {
      var raw = location.hash.replace(/^#\/?/, "");
      if (raw) {
        var parts = self._splitHash(raw);
        if (parts.path !== self._currentPath) {
          self._showItem(parts.path);
        }
        if (parts.anchor) {
          self._scrollToAnchor(parts.anchor);
        } else {
          // Clear anchor highlight when navigating without anchor
          self._targetStyle.textContent = "";
        }
        if (self._navEl) self._navEl.setAttribute("active", parts.path);
      } else {
        self._currentPath = null;
        self._showWelcome();
      }
    };
    window.addEventListener("hashchange", this._hashHandler);
  }

  _splitHash(raw) {
    var decoded = decodeURIComponent(raw);
    var tilde = decoded.indexOf("~");
    if (tilde !== -1) {
      return { path: decoded.substring(0, tilde), anchor: decoded.substring(tilde + 1) };
    }
    return { path: decoded, anchor: null };
  }

  // ---- Rendering ----

  _render() {
    if (this._headerEl) {
      var html = "";
      var backHref = this.getAttribute("back-href");
      var backLabel = this.getAttribute("back-label");
      if (backHref) {
        html += '<a class="fe-doc-viewer-back" href="' + _feViewerEsc(backHref) + '">' +
          _feViewerEsc(backLabel || "Back") + "</a>";
      }
      var title = this.getAttribute("title");
      if (title) {
        html += '<span class="fe-doc-viewer-title">' + _feViewerEsc(title) + "</span>";
      }
      this._headerEl.innerHTML = html;
    }

    if (this._navEl) {
      var attrs = ["filter", "filter-kind"];
      for (var i = 0; i < attrs.length; i++) {
        var val = this.getAttribute(attrs[i]);
        if (val) this._navEl.setAttribute(attrs[i], val);
        else this._navEl.removeAttribute(attrs[i]);
      }
      // Exclude always includes landing-page
      var exclude = this.getAttribute("exclude") || "";
      this._navEl.setAttribute("exclude", exclude ? exclude + ",landing-page" : "landing-page");
    }
  }

  // ---- Anchor Highlighting ----

  _scrollToAnchor(anchorId) {
    this._targetStyle.textContent = anchorId
      ? "#" + CSS.escape(anchorId) + " { background: var(--target-bg, rgba(99,102,241,0.08)); }"
      : "";

    // Retry a few times so the scroll lands even when the caller races the
    // content render (e.g. initial page load with #path~anchor in the URL —
    // _showItem rebuilds content asynchronously, so the target element may
    // not exist at the first tick).
    var self = this;
    var attempts = 0;
    function tryScroll() {
      if (!self._contentEl) return;
      var el = self._contentEl.querySelector("#" + CSS.escape(anchorId));
      if (el) {
        el.scrollIntoView({ behavior: "smooth", block: "start" });
        return;
      }
      attempts++;
      if (attempts < 20) setTimeout(tryScroll, 50);
    }
    setTimeout(tryScroll, 50);
  }

  // ---- SCIP Ambient Highlighting ----

  _applyDefaultHighlight(docPath) {
    var scip = this._scip || window.FE_SCIP;
    if (!scip || !docPath) {
      if (typeof feClearDefaultHighlight === "function") feClearDefaultHighlight();
      return;
    }
    var sym = scip.symbolForDocUrl(docPath);
    if (sym) {
      feSetDefaultHighlight(scip.symbolHash(sym));
    } else {
      if (typeof feClearDefaultHighlight === "function") feClearDefaultHighlight();
    }
  }

  // ---- Show Item ----

  _showItem(docPath) {
    if (!this._contentEl) return;
    var index = this._index || window.FE_DOC_INDEX;
    if (!index || !index.items) return;

    this._currentPath = docPath;
    this._targetStyle.textContent = "";

    if (this._navEl) this._navEl.setAttribute("active", docPath);

    // Find item
    var item = this._findByUrl(index, docPath);
    if (!item) {
      var slashIdx = docPath.lastIndexOf("/");
      var typePath = slashIdx !== -1 ? docPath.substring(0, slashIdx) : docPath;
      var tildeIdx = typePath.indexOf("~");
      if (tildeIdx !== -1) typePath = typePath.substring(0, tildeIdx);

      if (typePath.charAt(0) === "(") {
        this._contentEl.innerHTML = '<div class="generated-type">' +
          '<h1>Compiler-Generated Type</h1>' +
          '<fe-code-block lang="fe" class="signature">' + _feViewerEsc(typePath) + '</fe-code-block>' +
          '<p>This is a compiler-generated type. Tuple types like <code>' +
          _feViewerEsc(typePath) + '</code> are created automatically by the compiler ' +
          'and do not have dedicated documentation pages.</p></div>';
        this._clearOutline();
        return;
      }
      // Any unresolved simple type name (no :: path) is likely a builtin or
      // compiler-generated type (u8-u256, i8-i256, bool, isize, usize, etc.)
      if (typePath.indexOf("::") === -1 && /^[a-z]/.test(typePath)) {
        this._contentEl.innerHTML = '<div class="generated-type">' +
          '<h1>Built-in Type: <code>' + _feViewerEsc(typePath) + '</code></h1>' +
          '<p><code>' + _feViewerEsc(typePath) +
          '</code> is a built-in type provided by the compiler. ' +
          'Trait implementations for this type are generated automatically.</p></div>';
        this._clearOutline();
        return;
      }
      this._contentEl.innerHTML = '<div class="fe-doc-viewer-not-found">' +
        '<h1>Item Not Found</h1>' +
        '<p>The documentation item <code>' + _feViewerEsc(docPath) +
        '</code> could not be found.</p>' +
        '<p class="not-found-hint">It may have been renamed or removed.</p></div>';
      this._clearOutline();
      return;
    }

    // Render via <fe-doc-item>
    var docItem = document.createElement("fe-doc-item");
    docItem.setAttribute("symbol", item.path);
    // Exclude landing-page from item rendering too
    docItem.setAttribute("exclude", this.getAttribute("exclude")
      ? this.getAttribute("exclude") + ",landing-page" : "landing-page");
    if (this.getAttribute("src")) docItem.setAttribute("src", this.getAttribute("src"));
    if (this.getAttribute("base")) docItem.setAttribute("base", this.getAttribute("base"));

    var filterAttrs = ["filter", "filter-kind"];
    for (var i = 0; i < filterAttrs.length; i++) {
      var val = this.getAttribute(filterAttrs[i]);
      if (val) docItem.setAttribute(filterAttrs[i], val);
    }

    this._contentEl.innerHTML = "";
    this._contentEl.appendChild(docItem);
    this._contentEl.scrollTop = 0;

    // Update page title
    var viewerTitle = this.getAttribute("title") || "Fe Documentation";
    document.title = item.name + " \u2014 " + viewerTitle;

    // Build outline after content renders
    var self = this;
    setTimeout(function () {
      self._buildOutline();
      self._applyDefaultHighlight(docPath);
      // Scroll nav active item into view
      if (self._navEl) {
        var active = self._navEl.querySelector(".current a, .current");
        if (active) active.scrollIntoView({ block: "nearest" });
      }
    }, 150);
  }

  // ---- In-Page Outline / TOC ----

  _clearOutline() {
    if (this._outlineEl) this._outlineEl.innerHTML = "";
  }

  _buildOutline() {
    if (!this._outlineEl || !this._contentEl) return;
    this._outlineEl.innerHTML = "";

    var targets = this._contentEl.querySelectorAll(
      "h2[id], details.impl-block[id], details.method-item[id], div.method-item[id]"
    );
    if (targets.length === 0) return;

    var entries = [];
    for (var i = 0; i < targets.length; i++) {
      var el = targets[i];
      var id = el.id;
      if (!id) continue;
      var text = "";
      var level = 0;
      var methodDot = id.indexOf(".method.");

      if (el.tagName === "H2") {
        text = el.textContent.replace("\u00a7", "").trim();
        level = 0;
      } else if (methodDot !== -1) {
        text = id.substring(methodDot + 8) + "()";
        level = 2;
      } else if (el.classList.contains("impl-block")) {
        var h3 = el.querySelector("summary h3");
        text = h3 ? h3.textContent.trim() : id;
        if (text.length > 50) text = text.substring(0, 47) + "\u2026";
        level = 1;
      } else {
        var heading = el.querySelector("summary h3, summary h4");
        text = heading ? heading.textContent.trim()
          : (el.querySelector("summary") || el).textContent
              .replace("\u00a7", "").replace(/\u25b6/g, "").trim();
        if (text.length > 50) text = text.substring(0, 47) + "\u2026";
        level = 1;
      }
      if (id && text) entries.push({ id: id, text: text, level: level });
    }
    if (entries.length === 0) return;

    var header = document.createElement("h4");
    header.className = "outline-header";
    header.textContent = "On this page";
    this._outlineEl.appendChild(header);

    var list = document.createElement("ul");
    list.className = "outline-list";

    var path = this._currentPath || "";
    for (var j = 0; j < entries.length; j++) {
      var entry = entries[j];
      var li = document.createElement("li");
      if (entry.level > 0) li.className = "outline-level-" + entry.level;
      var a = document.createElement("a");
      a.href = "#" + path + "~" + entry.id;
      a.textContent = entry.text;
      a.dataset.outlineId = entry.id;
      li.appendChild(a);
      list.appendChild(li);
    }

    this._outlineEl.appendChild(list);
    this._syncOutlineHighlight();
  }

  _syncOutlineHighlight() {
    var raw = location.hash.replace(/^#\/?/, "");
    var parts = this._splitHash(raw);
    var anchor = parts.anchor;
    var links = this._outlineEl ? this._outlineEl.querySelectorAll(".outline-list a") : [];
    for (var i = 0; i < links.length; i++) {
      links[i].classList.toggle("active", anchor !== null && links[i].dataset.outlineId === anchor);
    }
  }

  // ---- Welcome / Landing ----

  _showWelcome() {
    if (!this._contentEl) return;
    var index = this._index || window.FE_DOC_INDEX;
    if (!index) {
      this._contentEl.innerHTML = '<div class="fe-doc-viewer-welcome">' +
        '<p>Loading documentation...</p></div>';
      return;
    }

    var modules = index.modules || [];
    var builtinModules = index.builtin_modules || [];
    var rootMod = modules[0] || builtinModules[0] || null;

    if (rootMod) {
      var modUrl = rootMod.path + "/mod";
      this._showItem(modUrl);
      if (this._navEl) this._navEl.setAttribute("active", modUrl);
      return;
    }

    var title = this.getAttribute("title") || "Fe Documentation";
    this._contentEl.innerHTML = '<div class="fe-doc-viewer-welcome">' +
      '<h1>' + _feViewerEsc(title) + '</h1>' +
      '<p>No documented items found.</p></div>';
    this._clearOutline();
  }

  // ---- URL Lookup ----

  _findByUrl(index, urlPath) {
    if (!urlPath) return null;
    var items = index.items || [];

    var slashIdx = urlPath.lastIndexOf("/");
    if (slashIdx !== -1) {
      var path = urlPath.substring(0, slashIdx);
      var kindSuffix = urlPath.substring(slashIdx + 1);
      var kindMap = {
        mod: "module", fn: "function", struct: "struct", enum: "enum",
        trait: "trait", contract: "contract", type: "type_alias",
        "const": "const", impl: "impl",
        msg: "msg", msg_variant: "msg_variant",
      };
      var kindName = kindMap[kindSuffix];
      if (kindName) {
        for (var i = 0; i < items.length; i++) {
          if (items[i].path === path && items[i].kind === kindName) return items[i];
        }
      }
    }

    for (var j = 0; j < items.length; j++) {
      if (items[j].path === urlPath) return items[j];
    }
    return null;
  }
}

function _feViewerEsc(s) {
  var d = document.createElement("div");
  d.textContent = s;
  return d.innerHTML;
}

customElements.define("fe-doc-viewer", FeDocViewer);
