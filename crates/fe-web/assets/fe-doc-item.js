// <fe-doc-item> — Self-contained documentation item renderer.
//
// Renders a complete doc item with signature, documentation, children
// (fields/variants/methods), trait implementations, implementors, and
// module members.  No external renderer needed.
//
// Usage:
//   <fe-doc-item symbol="mylib::Game/struct"></fe-doc-item>
//   <fe-doc-item src="/docs.json" symbol="core::option" filter-kind="struct,enum"></fe-doc-item>
//
// Attributes:
//   symbol      — doc path (e.g. "mylib::Game/struct" or "mylib::Game")
//   src         — URL to docs.json (uses shared fetch cache)
//   show-source — show the full source text if available
//   compact     — signature + summary only
//   filter      — comma-separated glob patterns for children
//   filter-kind — comma-separated child kinds to include
//   exclude     — comma-separated glob patterns to hide
//   base        — base URL for navigation links

// ============================================================================
// Kind metadata
// ============================================================================

var _ITEM_KIND = {
  module:     { str: "mod",      plural: "Modules",       display: "Module",      order: 0 },
  "function": { str: "fn",      plural: "Functions",      display: "Function",     order: 6 },
  struct:     { str: "struct",   plural: "Structs",        display: "Struct",       order: 3 },
  enum:       { str: "enum",     plural: "Enums",          display: "Enum",         order: 4 },
  trait:      { str: "trait",    plural: "Traits",         display: "Trait",        order: 1 },
  contract:   { str: "contract", plural: "Contracts",      display: "Contract",     order: 2 },
  type_alias: { str: "type",    plural: "Type Aliases",   display: "Type Alias",   order: 5 },
  "const":    { str: "const",   plural: "Constants",      display: "Constant",     order: 7 },
  impl:       { str: "impl",    plural: "Implementations", display: "Implementation", order: 8 },
  impl_trait: { str: "impl",    plural: "Trait Implementations", display: "Trait Implementation", order: 9 },
  msg:         { str: "msg",    plural: "Messages",       display: "Message",      order: 1 },
  msg_variant: { str: "msg_variant", plural: "Message Variants", display: "Message Variant", order: 3 },
};

var _CHILD_KIND = {
  field:        { plural: "Fields",              anchor: "field",            order: 1 },
  variant:      { plural: "Variants",            anchor: "variant",          order: 0 },
  method:       { plural: "Methods",             anchor: "tymethod",         order: 6 },
  assoc_type:   { plural: "Associated Types",    anchor: "associatedtype",   order: 4 },
  assoc_const:  { plural: "Associated Constants", anchor: "associatedconstant", order: 5 },
  // Contract-specific child kinds (emitted by crates/fe/src/extract.rs for
  // Contract items — see DocChildKind::{Init,RecvHandler}).
  init:         { plural: "Initializer",         anchor: "init",             order: 2 },
  recv_handler: { plural: "Message Handlers",    anchor: "handler",          order: 3 },
};

function _diKindStr(kind)     { return (_ITEM_KIND[kind] || {}).str || kind; }
function _diKindPlural(kind)  { return (_ITEM_KIND[kind] || {}).plural || kind; }
function _diKindDisplay(kind) { return (_ITEM_KIND[kind] || {}).display || kind; }
function _diKindOrder(kind)   { return (_ITEM_KIND[kind] || {}).order || 99; }

function _diEsc(s) {
  var d = document.createElement("div");
  d.textContent = s;
  return d.innerHTML;
}

function _diKindBadge(kind) {
  return '<span class="kind-badge ' + _diEsc(_diKindStr(kind)) + '">' + _diEsc(_diKindStr(kind)) + "</span>";
}

function _diGroupByKind(items, kindFn) {
  var groups = {}, order = {};
  for (var i = 0; i < items.length; i++) {
    var k = kindFn(items[i]);
    if (!groups[k.key]) {
      groups[k.key] = { kind: k.kind, plural: k.plural, items: [] };
      order[k.key] = k.order;
    }
    groups[k.key].items.push(items[i]);
  }
  var keys = Object.keys(groups);
  keys.sort(function (a, b) { return order[a] - order[b]; });
  return keys.map(function (k) { return groups[k]; });
}

// ============================================================================
// Component
// ============================================================================

class FeDocItem extends HTMLElement {
  static get observedAttributes() {
    return ["symbol", "src", "filter", "filter-kind", "exclude", "base", "compact", "show-source"];
  }

  attributeChangedCallback(name, oldVal, newVal) {
    if (oldVal === newVal) return;
    if (name === "src") { this._loadSrc(); return; }
    this._renderItem();
  }

  connectedCallback() {
    this._loadSrc();
    this._renderItem();
  }

  _loadSrc() {
    var src = this.getAttribute("src");
    if (!src) return;
    var self = this;
    feLoadSrc(src).then(function (result) {
      self._index = result.index;
      self._scip = result.scip;
      self._renderItem();
    });
  }

  _getIndex() {
    return this._index || window.FE_DOC_INDEX || { items: [], modules: [] };
  }

  _findItem(path) {
    var index = this._getIndex();
    if (!index.items) return null;
    for (var i = 0; i < index.items.length; i++) {
      if (index.items[i].path === path) return index.items[i];
    }
    return null;
  }

  _renderItem() {
    var symbolPath = this.getAttribute("symbol");
    if (!symbolPath) return;

    var index = this._getIndex();
    if (!index.items || index.items.length === 0) {
      if (!feWhenReady(this._renderItem.bind(this))) return;
      return;
    }

    var item = this._findItem(symbolPath);
    if (!item) {
      this.innerHTML = '<span class="fe-doc-item-error">Item not found: ' +
        _diEsc(symbolPath) + "</span>";
      return;
    }

    this.innerHTML = this._renderFull(item);
    this._refreshCodeBlocks();
  }

  // ---- Filtering ----

  _matchesFilter(name, path) {
    var filter = this.getAttribute("filter");
    if (!filter) return true;
    var patterns = filter.split(",");
    for (var i = 0; i < patterns.length; i++) {
      var pat = patterns[i].trim();
      if (!pat) continue;
      if (pat.endsWith("*")) {
        var prefix = pat.slice(0, -1);
        if ((path && path.indexOf(prefix) === 0) || name.indexOf(prefix) === 0) return true;
      } else {
        if (name === pat || path === pat) return true;
      }
    }
    return false;
  }

  _isExcluded(name, path) {
    var exclude = this.getAttribute("exclude");
    if (!exclude) return false;
    var patterns = exclude.split(",");
    for (var i = 0; i < patterns.length; i++) {
      var pat = patterns[i].trim();
      if (!pat) continue;
      if (pat.endsWith("*")) {
        var prefix = pat.slice(0, -1);
        if ((path && path.indexOf(prefix) === 0) || name.indexOf(prefix) === 0) return true;
      } else {
        if (name === pat || path === pat) return true;
      }
    }
    return false;
  }

  _matchesKind(kind) {
    var filterKind = this.getAttribute("filter-kind");
    if (!filterKind) return true;
    var kinds = filterKind.split(",");
    for (var i = 0; i < kinds.length; i++) {
      if (kinds[i].trim() === kind) return true;
    }
    return false;
  }

  _filterChildren(children) {
    if (!children) return [];
    var result = [];
    for (var i = 0; i < children.length; i++) {
      var child = children[i];
      if (!this._matchesFilter(child.name, child.path)) continue;
      if (this._isExcluded(child.name, child.path)) continue;
      if (!this._matchesKind(child.kind)) continue;
      result.push(child);
    }
    return result;
  }

  _filterTraitImpls(impls) {
    if (!impls) return [];
    var result = [];
    for (var i = 0; i < impls.length; i++) {
      if (impls[i].trait_name && this._isExcluded(impls[i].trait_name, "")) continue;
      result.push(impls[i]);
    }
    return result;
  }

  // ---- Full rendering ----

  _renderFull(item) {
    var compact = this.hasAttribute("compact");
    var showSource = this.hasAttribute("show-source");
    var isModule = item.kind === "module";
    var parentUrl = item.path + "/" + _diKindStr(item.kind);

    var html = '<article class="doc-item">';

    // Breadcrumbs
    html += this._renderBreadcrumbs(item);

    // Header
    html += '<div class="item-header"><div class="item-title">';
    html += '<span class="kind-badge ' + _diEsc(_diKindStr(item.kind)) + '">' +
      _diEsc(_diKindDisplay(item.kind)) + "</span>";
    html += '<h1>' + _diEsc(item.name) +
      '<a href="' + this._itemHref(parentUrl) + '" class="anchor">\u00a7</a></h1>';
    html += "</div></div>";

    // Source link
    if (item.source_text && item.source) {
      html += '<details class="source-toggle"><summary class="src-link">';
      html += _diEsc(item.source.display_file || "");
      if (item.source.line) html += ':' + item.source.line;
      html += '</summary>';
      html += '<fe-code-block lang="fe" line-numbers';
      if (item.source.display_file) html += ' data-file="' + _diEsc(item.source.display_file) + '"';
      if (item.source.line) html += ' data-line-offset="' + (item.source.line - 1) + '"';
      html += '>' + _diEsc(item.source_text) + '</fe-code-block>';
      html += '</details>';
    } else if (item.source && item.source.display_file) {
      html += '<div class="src-link">' + _diEsc(item.source.display_file);
      if (item.source.line) html += ':' + item.source.line;
      html += '</div>';
    }

    // Signature (non-modules)
    if (!isModule && item.signature && !compact) {
      html += '<div class="signature-wrapper">';
      html += this._renderSignature(item);
      html += '<a href="' + this._itemHref(parentUrl) + '" class="anchor">\u00a7</a>';
      html += '</div>';
    }

    // Documentation body
    if (item.docs) {
      html += this._renderDocContent(item.docs, compact);
    }

    if (compact) {
      html += "</article>";
      return html;
    }

    // Module members
    if (isModule) {
      html += this._renderModuleMembers(item);
    }

    // Children (fields, variants, methods)
    var children = this._filterChildren(item.children);
    if (children.length > 0) {
      html += this._renderChildren(children, parentUrl);
    }

    // Trait implementations
    var impls = this._filterTraitImpls(item.trait_impls);
    if (impls.length > 0) {
      html += this._renderTraitImpls(impls, parentUrl);
    }

    // Implementors (for trait pages)
    if (item.implementors && item.implementors.length > 0) {
      html += this._renderImplementors(item.implementors, parentUrl);
    }

    html += "</article>";
    return html;
  }

  /** Generate href for a doc path. */
  _itemHref(docPath) {
    var base = this.getAttribute("base") || "";
    return base ? base + "#" + docPath : "#" + docPath;
  }

  /** Generate href for an in-page anchor (parentUrl~anchorId). */
  _anchorHref(parentUrl, anchorId) {
    return this._itemHref(parentUrl + "~" + anchorId);
  }

  _renderBreadcrumbs(item) {
    var segments = item.path.split("::");
    var index = this._getIndex();
    var items = (index && index.items) || [];
    var html = '<nav class="breadcrumb">';
    var accumulated = "";
    for (var i = 0; i < segments.length; i++) {
      if (i > 0) {
        accumulated += "::";
        html += '<span class="breadcrumb-sep">::</span>';
      }
      accumulated += segments[i];
      if (i === segments.length - 1) {
        html += '<span class="breadcrumb-current">' + _diEsc(segments[i]) + "</span>";
      } else {
        // Resolve the real kind for each ancestor segment so non-module
        // containers (e.g. `msg`) link to their actual page, not `/mod`.
        var suffix = "mod";
        for (var k = 0; k < items.length; k++) {
          if (items[k].path === accumulated) {
            suffix = _diKindStr(items[k].kind);
            break;
          }
        }
        html += '<a href="' + _diEsc(this._itemHref(accumulated + "/" + suffix)) +
          '" class="breadcrumb-link">' + _diEsc(segments[i]) + "</a>";
      }
    }
    html += "</nav>";
    return html;
  }

  _renderSignature(item) {
    var attrs = 'lang="fe"';
    if (item.sig_scope) attrs += ' data-scope="' + _diEsc(item.sig_scope) + '"';
    attrs += ' class="signature"';
    return "<fe-code-block " + attrs + ">" + _diEsc(item.signature || "") + "</fe-code-block>";
  }

  _renderDocContent(docs, compact) {
    var html = '<div class="docs">';
    var bodyHtml = docs.html_body || _diEsc(docs.body || "");
    html += bodyHtml;

    if (!compact && docs.sections && docs.sections.length > 0) {
      for (var i = 0; i < docs.sections.length; i++) {
        var section = docs.sections[i];
        var sectionId = "section-" + section.name.toLowerCase().replace(/\s+/g, "-");
        html += '<div class="doc-section" id="' + _diEsc(sectionId) + '">';
        html += '<div class="doc-section-badge">' + _diEsc(section.name) + "</div>";
        var sectionHtml = section.html_content || _diEsc(section.content || "");
        html += '<div class="doc-section-content">' + sectionHtml + "</div>";
        html += "</div>";
      }
    }

    html += "</div>";
    return html;
  }

  _renderChildren(children, parentUrl) {
    var grouped = _diGroupByKind(children, function (child) {
      var info = _CHILD_KIND[child.kind] || { plural: child.kind, anchor: child.kind, order: 99 };
      return { key: child.kind, kind: child.kind, plural: info.plural, order: info.order };
    });

    var html = '<div class="children-sections">';
    for (var g = 0; g < grouped.length; g++) {
      var group = grouped[g];
      var info = _CHILD_KIND[group.kind] || { anchor: group.kind };
      var sectionId = info.anchor + "s";
      html += '<section class="children-section">';
      html += '<h2 id="' + _diEsc(sectionId) + '">' + _diEsc(group.plural) +
        '<a href="' + this._anchorHref(parentUrl, sectionId) + '" class="anchor">\u00a7</a></h2>';
      html += '<div class="member-list">';
      for (var j = 0; j < group.items.length; j++) {
        var child = group.items[j];
        var anchorId = info.anchor + "." + child.name;
        var rowHref = this._anchorHref(parentUrl, anchorId);
        html += '<div class="member-item" id="' + _diEsc(anchorId) + '">';
        html += '<a class="member-header-link" href="' + _diEsc(rowHref) + '">';
        html += '<div class="member-header" data-row-link="true">';
        html += this._renderChildSignature(child);
        // Nested <a> is invalid HTML, so use a <span> for the visual anchor
        // glyph. Clicking anywhere in the header (including this glyph)
        // navigates via the outer <a>.
        html += '<span class="anchor" aria-hidden="true">\u00a7</span>';
        html += "</div>";
        html += "</a>";
        if (child.docs) {
          var childHtml = child.docs.html_body || _diEsc(child.docs.body || child.docs.summary || "");
          html += '<div class="member-docs">' + childHtml + "</div>";
        }
        html += "</div>";
      }
      html += "</div></section>";
    }
    html += "</div>";
    return html;
  }

  _renderChildSignature(child) {
    var sig = child.signature || child.name;
    var attrs = 'lang="fe"';
    if (child.sig_scope) attrs += ' data-scope="' + _diEsc(child.sig_scope) + '"';
    return "<fe-code-block " + attrs + ">" + _diEsc(sig) + "</fe-code-block>";
  }

  _renderTraitImpls(impls, parentUrl) {
    var traitImpls = [], inherentImpls = [];
    for (var i = 0; i < impls.length; i++) {
      if (impls[i].trait_name) traitImpls.push(impls[i]);
      else inherentImpls.push(impls[i]);
    }

    var html = '<div class="implementations">';

    if (inherentImpls.length > 0) {
      html += '<section class="inherent-impls">';
      html += '<h2 id="implementations">Implementations' +
        '<a href="' + this._anchorHref(parentUrl, "implementations") + '" class="anchor">\u00a7</a></h2>';
      html += '<div class="impl-list">';
      for (var ii = 0; ii < inherentImpls.length; ii++) {
        html += this._renderImplBlock(inherentImpls[ii], "impl-" + ii, parentUrl);
      }
      html += "</div></section>";
    }

    if (traitImpls.length > 0) {
      html += '<section class="trait-impls">';
      html += '<h2 id="trait-implementations">Trait Implementations' +
        '<a href="' + this._anchorHref(parentUrl, "trait-implementations") + '" class="anchor">\u00a7</a></h2>';
      html += '<div class="impl-list">';
      for (var ti = 0; ti < traitImpls.length; ti++) {
        var anchorId = "impl-" + traitImpls[ti].trait_name.replace(/[<> ,]/g, "_");
        html += this._renderImplBlock(traitImpls[ti], anchorId, parentUrl);
      }
      html += "</div></section>";
    }

    html += "</div>";
    return html;
  }

  _renderImplBlock(impl_, anchorId, parentUrl) {
    var isTraitImpl = !!impl_.trait_name;
    var headerDisplay = isTraitImpl ? "impl " + impl_.trait_name : impl_.signature;

    var html = '<details class="impl-block toggle" open id="' + _diEsc(anchorId) + '">';
    html += "<summary><span class=\"impl-header\">";
    html += "<h3><code>" + _diEsc(headerDisplay) + "</code></h3>";
    html += '<a href="' + this._anchorHref(parentUrl, anchorId) + '" class="anchor">\u00a7</a>';
    html += "</span></summary>";
    html += '<div class="impl-content">';

    if (isTraitImpl && impl_.signature) {
      var attrs = 'lang="fe" class="impl-signature"';
      if (impl_.sig_scope) attrs += ' data-scope="' + _diEsc(impl_.sig_scope) + '"';
      html += "<fe-code-block " + attrs + ">" + _diEsc(impl_.signature) + "</fe-code-block>";
    }

    var methods = this._filterChildren(impl_.methods);
    if (methods.length > 0) {
      html += '<div class="impl-items">';
      for (var m = 0; m < methods.length; m++) {
        var methodAnchor = anchorId + ".method." + methods[m].name;
        html += this._renderMethodItem(methods[m], methodAnchor, parentUrl);
      }
      html += "</div>";
    }

    html += "</div></details>";
    return html;
  }

  _renderMethodItem(method, anchorId, parentUrl) {
    var sigAttrs = 'lang="fe"';
    if (method.sig_scope) sigAttrs += ' data-scope="' + _diEsc(method.sig_scope) + '"';
    var anchorLink = '<a href="' + this._anchorHref(parentUrl, anchorId) + '" class="anchor">\u00a7</a>';
    var headerHtml = '<div class="method-header">' +
      '<h4 class="code-header"><fe-code-block ' + sigAttrs + '>' +
      _diEsc(method.signature || method.name) + '</fe-code-block></h4>' +
      anchorLink + '</div>';

    if (method.docs) {
      var docsHtml = method.docs.html_body || _diEsc(method.docs.body || method.docs.summary || "");
      return '<details class="method-item toggle" open id="' + _diEsc(anchorId) + '">' +
        "<summary>" + headerHtml + "</summary>" +
        '<div class="method-docblock">' + docsHtml + "</div></details>";
    }
    return '<div class="method-item no-toggle" id="' + _diEsc(anchorId) + '">' + headerHtml + "</div>";
  }

  _renderImplementors(implementors, parentUrl) {
    var html = '<section class="implementors">';
    html += '<h2 id="implementors">Implementors' +
      '<a href="' + this._anchorHref(parentUrl, "implementors") + '" class="anchor">\u00a7</a></h2>';
    html += '<div class="implementor-list">';
    for (var i = 0; i < implementors.length; i++) {
      var imp = implementors[i];
      var anchorId = "impl-" + imp.type_name.replace(/[<> ,]/g, "_");
      html += '<div class="implementor-item" id="' + _diEsc(anchorId) + '">';
      var sigAttrs = 'lang="fe"';
      if (imp.sig_scope) sigAttrs += ' data-scope="' + _diEsc(imp.sig_scope) + '"';
      html += '<fe-code-block ' + sigAttrs + ' class="implementor-sig">' +
        _diEsc(imp.signature || "") + '</fe-code-block>';
      // ↪ link to the implementation on the type's page
      if (imp.type_url && imp.trait_name) {
        var implTarget = imp.type_url + "~impl-" + imp.trait_name.replace(/[<> ,]/g, "_");
        html += '<a href="' + this._itemHref(implTarget) + '" class="impl-go" title="Go to implementation">\u21AA</a>';
      }
      html += '<a href="' + this._anchorHref(parentUrl, anchorId) + '" class="anchor">\u00a7</a>';
      html += "</div>";
    }
    html += "</div></section>";
    return html;
  }

  _renderModuleMembers(item) {
    var index = this._getIndex();
    var modContent = this._findModuleContent(index.modules || [], item.path)
      || this._findModuleContent(index.builtin_modules || [], item.path);
    if (!modContent) return "";

    var items = modContent.items;
    var submodules = modContent.submodules;
    if ((!submodules || submodules.length === 0) && (!items || items.length === 0)) return "";

    var base = this.getAttribute("base") || "";
    var html = '<div class="module-items">';

    if (submodules && submodules.length > 0) {
      html += '<section class="item-table" id="modules">';
      html += "<h2>Modules</h2>";
      html += '<div class="item-list">';
      for (var s = 0; s < submodules.length; s++) {
        if (this._isExcluded(submodules[s].name, submodules[s].path)) continue;
        var href = this._itemHref(submodules[s].path + "/mod");
        var modSummary = this._getModuleSummary(index, submodules[s].path);
        html += '<div class="item-row">';
        html += '<div class="item-name">' + _diKindBadge("mod") +
          '<a href="' + _diEsc(href) + '"><code>' + _diEsc(submodules[s].name) + "</code></a></div>";
        html += '<div class="item-summary">' + _diEsc(modSummary) + "</div>";
        html += "</div>";
      }
      html += "</div></section>";
    }

    if (items && items.length > 0) {
      // Apply filtering
      var filtered = [];
      for (var f = 0; f < items.length; f++) {
        if (this._isExcluded(items[f].name, items[f].path)) continue;
        if (!this._matchesKind(items[f].kind)) continue;
        filtered.push(items[f]);
      }

      var grouped = _diGroupByKind(filtered, function (it) {
        return {
          key: it.kind,
          kind: _diKindStr(it.kind),
          plural: _diKindPlural(it.kind),
          order: _diKindOrder(it.kind),
        };
      });

      for (var g = 0; g < grouped.length; g++) {
        var group = grouped[g];
        html += '<section class="item-table">';
        html += "<h2>" + _diEsc(group.plural) + "</h2>";
        html += '<div class="item-list">';
        for (var j = 0; j < group.items.length; j++) {
          var it = group.items[j];
          var url = it.path + "/" + _diKindStr(it.kind);
          var itemHref = this._itemHref(url);
          html += '<div class="item-row">';
          html += '<div class="item-name">' + _diKindBadge(_diKindStr(it.kind)) +
            '<a href="' + _diEsc(itemHref) + '"><code>' + _diEsc(it.name) + "</code></a></div>";
          html += '<div class="item-summary">' + _diEsc(it.summary || "") + "</div>";
          html += "</div>";
        }
        html += "</div></section>";
      }
    }

    html += "</div>";
    return html;
  }

  /** Look up a module's doc summary from the index items list. */
  _getModuleSummary(index, modulePath) {
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

  _findModuleContent(modules, path) {
    for (var i = 0; i < modules.length; i++) {
      var mod = modules[i];
      if (mod.path === path) {
        var submodules = (mod.children || []).map(function (c) {
          return { name: c.name, path: c.path };
        });
        return { items: mod.items || [], submodules: submodules };
      }
      if (mod.children) {
        var found = this._findModuleContent(mod.children, path);
        if (found) return found;
      }
    }
    return null;
  }

  _refreshCodeBlocks() {
    var blocks = this.querySelectorAll("fe-code-block");
    for (var i = 0; i < blocks.length; i++) {
      if (blocks[i].refresh) blocks[i].refresh();
    }
  }
}

customElements.define("fe-doc-item", FeDocItem);
