// <fe-origin-trace> — Interactive trace-through graph viewer.
//
// The component expects `window.FE_ORIGIN_TRACE_DATA` to contain a derived trace
// view model. Regions across panels are connected by CSS classes named
// `trace-c-*`, mirroring the SCIP def/ref highlighting strategy used by
// <fe-code-block>.

(function () {
  "use strict";

  function el(tag, cls, text) {
    var node = document.createElement(tag);
    if (cls) node.className = cls;
    if (text !== undefined && text !== null) node.textContent = String(text);
    return node;
  }

  function traceClasses(node) {
    if (!node) return [];
    if (node.__traceClasses) return node.__traceClasses;
    if (node.dataset && node.dataset.traceGroups) {
      node.__traceClasses = node.dataset.traceGroups.split(/\s+/).filter(Boolean);
      return node.__traceClasses;
    }
    return Array.prototype.filter.call(node.classList || [], function (name) {
      return isTraceGroup(name);
    });
  }

  function isTraceGroup(name) {
    return /^(trace|exact|generated|prepared|context|structural)-c-/.test(name || "");
  }

  function allClasses(node) {
    return Array.prototype.slice.call(node.classList || []);
  }

  function keyClass(key) {
    var hash = 2166136261;
    for (var i = 0; i < key.length; i++) {
      hash ^= key.charCodeAt(i);
      hash = Math.imul(hash, 16777619);
    }
    return "trace-key-" + (hash >>> 0).toString(16);
  }

  class FeOriginTrace extends HTMLElement {
    connectedCallback() {
      if (!this.shadowRoot) this.attachShadow({ mode: "open" });
      this._data = window.FE_ORIGIN_TRACE_DATA || {};
      this._selected = [];
      this._selectedDisplayClasses = [];
      this._paneChoices = null;
      this._displayMode = "compact";
      this._hovered = [];
      this._pendingHover = null;
      this._hoverFrame = 0;
      this._stateStyle = null;
      if (!this._boundHashChange) {
        this._boundHashChange = this._handleHashChange.bind(this);
        this._boundPopState = this._handleHashChange.bind(this);
        window.addEventListener("hashchange", this._boundHashChange);
        window.addEventListener("popstate", this._boundPopState);
      }
      this._render();
    }

    disconnectedCallback() {
      this._hashNavigationToken = (this._hashNavigationToken || 0) + 1;
      if (this._boundHashChange) {
        window.removeEventListener("hashchange", this._boundHashChange);
        this._boundHashChange = null;
      }
      if (this._boundPopState) {
        window.removeEventListener("popstate", this._boundPopState);
        this._boundPopState = null;
      }
      this._interactionsInstalled = false;
    }

    setTraceData(data) {
      var scrollState = this._capturePaneScrollState ? this._capturePaneScrollState() : null;
      this._data = data || {};
      this._render({ scrollState: scrollState, switchedPaneIndex: null });
    }

    _render(scrollRestore) {
      var data = this._data;
      this.shadowRoot.textContent = "";
      this.shadowRoot.append(el("style", "", this._style()));
      this._stateStyle = el("style");
      this.shadowRoot.append(this._stateStyle);

      var page = el("div", "trace-page");
      var header = el("header", "trace-header");
      var controls = el("div", "header-controls");
      var mode = el("select", "display-mode-select");
      ["compact", "annotated", "debug"].forEach(function (value) {
        var option = el("option", "", value === "debug" ? "Debug/raw" : value[0].toUpperCase() + value.slice(1));
        option.value = value;
        option.selected = value === this._displayMode;
        mode.append(option);
      }, this);
      controls.append(mode);
      header.append(
        el("h1", "", "Fe Trace Workbench"),
        el(
          "p",
          "subtitle",
          this._provenanceCopy()
        ),
        controls
      );
      page.append(header);

      var cards = this._topCards();
      if (cards) page.append(cards);
      if (this._displayMode === "debug") {
        var internals = el("section", "cards internals");
        this._card(internals, "facts", data.counts && data.counts.facts);
        this._card(internals, "evidence paths", (data.closures || []).length);
        this._card(internals, "needs evidence", data.audit && data.audit.suspicious_closures);
        this._card(internals, "mixed spans", data.audit && data.audit.span_groups && data.audit.span_groups.mixed_connectivity_groups);
        if (data.salsa) {
          this._card(internals, "salsa mode", data.salsa.mode);
          this._card(internals, "query execs", data.salsa.will_execute);
          this._card(internals, "memo reuse", data.salsa.memo_reuse);
          this._card(internals, "render ms", data.salsa.elapsed_ms);
        }
        page.append(internals);
      }

      var workspace = this._workbench();
      page.append(workspace);

      var bottom = el("section", "bottom-deck");
      bottom.append(this._detailPanel());
      if (data.attribution_audit) {
        bottom.append(this._missingLinkAudit(data.attribution_audit));
      }
      if (data.static_analysis) {
        bottom.append(this._analysis(data.static_analysis));
      }
      page.append(bottom);

      var notes = el("ul", "notes");
      (data.notes || []).forEach(function (note) {
        notes.append(el("li", "", note));
      });
      page.append(notes);
      this.shadowRoot.append(page);
      this._installInteractions();
      this._restorePaneScroll(scrollRestore);
      this._renderDetail(this._selected);
      this._syncStateStyles();
      if (window.location.hash) this._scheduleHashNavigation();
    }

    _card(parent, label, value) {
      var card = el("div", "card");
      card.append(el("span", "", label), el("b", "", value == null ? "unknown" : value));
      parent.append(card);
    }

    _topCards() {
      var data = this._data || {};
      var cards = el("section", "cards");
      this._card(cards, "data", data.metadata && data.metadata.data_source);
      this._card(cards, "optimization", this._optimizationFlag());
      this._card(cards, "provenance", data.provenance && data.provenance.summary);
      this._card(cards, "missing links", this._missingLinkSummary());
      this._card(cards, "bytecode", this._bytecodeSummary());
      this._card(cards, "source spans", data.source && data.source.confidence);
      return cards;
    }

    _missingLinkSummary() {
      var audit = this._data && this._data.attribution_audit;
      if (!audit) return "unknown";
      var count = audit.missing_optimized_to_prepared_lineage_pcs || 0;
      if (count === 0) return "none detected";
      return count + " prepared PC" + (count === 1 ? "" : "s");
    }

    _missingLinkAudit(audit) {
      var section = el("section", "analysis missing-link-audit");
      var count = (audit && audit.missing_optimized_to_prepared_lineage_pcs) || 0;
      section.append(
        el("h2", "", "Missing Link Report"),
        el(
          "p",
          "muted",
          "Checks for prepared-linked bytecode whose EVM prepared/codegen origin lacks a validated optimized-Sonatina lineage edge."
        )
      );
      var summary = el("div", "missing-link-summary");
      summary.append(
        this._metric("source-exact", audit.source_exact_pcs || 0),
        this._metric("optimized-linked", audit.optimized_sonatina_linked_pcs || 0),
        this._metric("prepared-linked", audit.prepared_linked_pcs || 0),
        this._metric("lineage gaps", count),
        this._metric("unmapped", audit.unmapped_pcs || 0)
      );
      section.append(summary);
      if (count > 0) {
        section.append(el("p", "gap", "Prepared bytecode exists, but " + count + " bytecode PC(s) lack an explicit optimized Sonatina → EVM prepared bridge. This is missing evidence, not automatically a compiler bug."));
      } else {
        section.append(el("p", "audit-note", "No optimized→prepared lineage gaps were detected by this report."));
      }
      var targets = (audit.missing_lineage_targets || []).slice(0, 5);
      if (targets.length) {
        var list = el("div", "missing-targets");
        list.append(el("h3", "", "Top prepared targets without optimized lineage"));
        targets.forEach(function (target) {
          var row = el("div", "audit-count");
          row.append(el("span", "", this._compactLabel(this._originDisplay(target.target))), el("b", "", target.count || 0));
          list.append(row);
        }, this);
        section.append(list);
      }
      return section;
    }

    _originDisplay(origin) {
      if (!origin) return "";
      if (typeof origin === "string") return origin;
      if (origin.kind || origin.owner || origin.local) {
        return [origin.kind, origin.owner, origin.local].filter(Boolean).join(":");
      }
      return String(origin);
    }

    _metric(label, value) {
      var node = el("div", "metric");
      node.append(el("span", "", label), el("b", "", value));
      return node;
    }

    _provenanceCopy() {
      var data = this._data || {};
      var p = data.provenance || {};
      return "Source → Optimized Sonatina: " + (p.source_to_optimized || "unknown")
        + " · Optimized Sonatina → EVM prepared: " + (p.optimized_to_prepared || "unknown")
        + " · EVM prepared → Bytecode: " + (p.prepared_to_bytecode || "unknown");
    }

    _optimizationFlag() {
      var flags = (this._data.metadata && this._data.metadata.flags) || [];
      var flag = flags.filter(function (value) { return String(value).indexOf("optimize=") === 0; })[0];
      return flag ? flag.replace("optimize=", "O") : "unknown";
    }

    _bytecodeSummary() {
      var rows = 0;
      (this._data.panels || []).forEach(function (panel) {
        if (panel.id === "bytecode") rows = (panel.rows || []).length;
      });
      return rows + " ops";
    }

    _analysis(report) {
      var section = el("section", "analysis");
      var head = el("div", "analysis-head");
      head.append(
        el("h2", "", "Trace Health"),
        el("p", "muted", "Plain-language summaries over the emitted trace bundle. Inconclusive means the trace lacks evidence; it is not a compiler-correctness claim.")
      );
      section.append(head);

      var checks = el("div", "analysis-grid");
      (report.checks || []).forEach(function (check) {
        var card = el("div", "analysis-card status-" + (check.status || "unknown"));
        card.append(
          el("h3", "", this._friendlyCheckTitle(check)),
          el("p", "analysis-status", "status: " + (check.status || "unknown") + " · confidence: " + (check.confidence || "unknown")),
          el("p", "muted", check.summary || "")
        );
        if ((check.gaps || []).length) {
          card.append(el("p", "gap", "needs more compiler evidence: " + (check.gaps || []).length + " gap(s)"));
        }
        checks.append(card);
      }, this);
      section.append(checks);

      var bloat = report.bloat;
      if (bloat && (bloat.findings || []).length) {
        var bloatBox = el("div", "bloat-card");
        bloatBox.append(
          el("h3", "", "Bloat Signals"),
          el("p", "muted", (bloat.total_instructions || 0) + " bytecode instruction(s) · " + (bloat.total_byte_len || 0) + " byte(s) · " + (bloat.total_static_gas || 0) + " static gas")
        );
        (bloat.findings || []).slice(0, 5).forEach(function (finding) {
          var row = el("div", "bloat-row");
          row.append(
            el("span", "bloat-kind", (finding.kind || "").replace(/_/g, " ")),
            el("span", "bloat-count", (finding.instruction_count || 0) + " ops · " + (finding.byte_len || 0) + " bytes · " + (finding.static_gas || 0) + " gas")
          );
          bloatBox.append(row);
        });
        section.append(bloatBox);
      }
      return section;
    }

    _friendlyCheckTitle(check) {
      var id = (check && check.check_id) || "";
      var title = (check && check.title) || id;
      if (id.indexOf("provenance") >= 0) return "Can bytecode be explained?";
      if (id.indexOf("postopt") >= 0 || id.indexOf("attribution_gap") >= 0) return "Did optimized IR reach bytecode?";
      if (id.indexOf("bloat") >= 0) return "Where did bytecode size accumulate?";
      return title;
    }

    _representations() {
      var source = this._data.source || {};
      var reps = [{
        id: "source",
        title: "Source Syntax",
        summary: source.display_name || "source",
        kind: "source",
        source: source,
      }];
      (this._data.panels || []).forEach(function (panel) {
        reps.push({
          id: panel.id,
          title: panel.id === "sonatina-post" ? "Optimized Sonatina" : panel.title,
          summary: panel.summary,
          kind: "panel",
          panel: panel,
        });
      });
      return reps;
    }

    _defaultPaneChoices(reps) {
      function has(id) {
        return reps.some(function (rep) { return rep.id === id; });
      }
      var choices = [
        has("source") ? "source" : (reps[0] && reps[0].id),
        has("sonatina-post") ? "sonatina-post" : (has("sonatina-pre") ? "sonatina-pre" : (reps[1] && reps[1].id)),
        has("bytecode") ? "bytecode" : (reps[2] && reps[2].id),
      ].filter(Boolean);
      while (choices.length < 3 && reps[0]) choices.push(reps[0].id);
      return choices;
    }

    _workbench() {
      var reps = this._representations();
      if (!this._paneChoices || this._paneChoices.length !== 3) {
        this._paneChoices = this._defaultPaneChoices(reps);
      }
      var workspace = el("main", "workspace");
      var deck = el("section", "pane-deck");
      for (var i = 0; i < 3; i++) {
        deck.append(this._workbenchPane(i, reps));
      }
      workspace.append(deck);
      return workspace;
    }

    _workbenchPane(index, reps) {
      var selectedId = this._paneChoices[index] || (reps[0] && reps[0].id);
      var selected = reps.filter(function (rep) { return rep.id === selectedId; })[0] || reps[0];
      var panel = el("section", "panel workbench-pane");
      panel.dataset.paneIndex = String(index);
      if (selected) panel.dataset.representationId = selected.id;
      var head = el("div", "panel-head workbench-head");
      var select = el("select", "representation-select");
      select.dataset.paneIndex = String(index);
      reps.forEach(function (rep) {
        var option = el("option", "", rep.title);
        option.value = rep.id;
        option.selected = rep.id === selected.id;
        select.append(option);
      });
      var jump = el("div", "jump-controls");
      var prev = el("button", "jump-button", "Prev");
      var next = el("button", "jump-button", "Next");
      prev.type = "button";
      next.type = "button";
      prev.dataset.paneJump = "prev";
      next.dataset.paneJump = "next";
      jump.append(prev, next);
      head.append(el("h2", "", selected ? selected.title : "Representation"), select, jump);
      if (selected && selected.summary) head.append(el("p", "", selected.summary));
      panel.append(head);
      if (!selected) {
        panel.append(el("p", "empty-pane", "No representation available."));
      } else if (selected.kind === "source") {
        panel.append(this._sourceBody(selected.source || {}));
      } else {
        panel.append(this._panelBody(selected.panel || {}));
      }
      return panel;
    }

    _sourceBody(source) {
      var body = el("div", "source-lines");
      var markers = [];
      var lines = source.lines || [];
      var related = source.related_sources || [];
      var markerTotal = lines.length + related.reduce(function (sum, section) {
        return sum + ((section && section.lines) || []).length;
      }, 0);
      var markerIndex = 0;
      lines.forEach(function (line, index) {
        var classes = line.classes || [];
        var row = el("div", "source-line trace-region " + classes.concat(this._auditClasses(classes)).join(" "));
        this._bindTraceGroups(row, classes);
        row.id = line.row_id || this._rowId("source-main", index);
        row.dataset.sourceRef = "main:" + line.number;
        row.dataset.traceLabel = "source line " + line.number;
        row.append(el("span", "ln", line.number), el("code", "", line.text), this._badges(line));
        body.append(row);
        var marker = this._overviewMarker(classes, markerIndex, markerTotal, "source line " + line.number, row.id);
        if (marker) markers.push(marker);
        markerIndex += 1;
      }, this);
      related.forEach(function (section, sectionIndex) {
        var header = el("div", "source-section-separator");
        header.append(el("span", "", section.display_name || "related source"), el("small", "", section.origin || ""));
        body.append(header);
        ((section && section.lines) || []).forEach(function (line, lineIndex) {
          var classes = line.classes || [];
          var row = el("div", "source-line related-source-line trace-region " + classes.concat(this._auditClasses(classes)).join(" "));
          this._bindTraceGroups(row, classes);
          row.id = line.row_id || this._rowId("source-related-" + sectionIndex, lineIndex);
          row.dataset.sourceRef = "related:" + sectionIndex + ":" + line.number;
          row.dataset.traceLabel = (section.display_name || "related source") + " line " + line.number;
          row.append(el("span", "ln", line.number), el("code", "", line.text), this._badges(line));
          body.append(row);
          var marker = this._overviewMarker(classes, markerIndex, markerTotal, row.dataset.traceLabel, row.id);
          if (marker) markers.push(marker);
          markerIndex += 1;
        }, this);
      }, this);
      if (!lines.length && !related.length) {
        body.append(el("p", "empty-pane", "No source rows available."));
      }
      return this._listingShell(body, markers);
    }

    _panelBody(panelData) {
      var rows = el("div", "rows");
      var markers = [];
      var panelRows = panelData.rows || [];
      panelRows.forEach(function (rowData, index) {
        var boundaryLabel = this._rowBoundaryLabel(panelData, rowData, index);
        if (boundaryLabel) {
          rows.append(el("div", "boundary-marker", boundaryLabel));
        }
        var classes = rowData.classes || [];
        var hasTrace = traceClasses({ classList: classes }).length > 0;
        var kind = rowData.kind || (this._isBoundaryRow(rowData) ? "block_header" : "instruction");
        var rowKind = "row-kind-" + String(kind).replace(/_/g, "-") + " " + (this._isBoundaryKind(kind) ? "boundary-row " : "");
        var row = el("button", "trace-row trace-region " + rowKind + (hasTrace ? "" : "unlinked-row ") + classes.concat(this._auditClasses(classes)).join(" "));
        this._bindTraceGroups(row, classes);
        row.type = "button";
        row.id = rowData.row_id || this._rowId("panel-" + (panelData.id || "panel"), index);
        row.dataset.rowKind = kind;
        row.style.setProperty("--row-indent", String(rowData.indent || 0));
        if (rowData.key) {
          row.dataset.originKey = rowData.key;
          row.classList.add(keyClass(rowData.key));
        }
        var text = rowData.compact_text || rowData.text || "";
        row.dataset.traceLabel = rowData.label || text || rowData.key || "";
        row.append(
          el("span", "row-label", rowData.label),
          el("span", "row-meta", rowData.meta),
          el("span", "row-text", text),
          this._badges(rowData)
        );
        rows.append(row);
        var marker = this._overviewMarker(classes, index, panelRows.length, rowData.label || text || rowData.key || "", row.id);
        if (marker) markers.push(marker);
      }, this);
      if (!panelRows.length) {
        rows.append(el("p", "empty-pane", "No rows for this representation. If this is Bytecode, exact PC attribution may be unavailable for the selected trace."));
      }
      return this._listingShell(rows, markers);
    }

    _listingShell(body, markers) {
      var shell = el("div", "listing-shell");
      var overview = el("div", "overview-rail");
      if (markers.length) {
        markers.forEach(function (marker) { overview.append(marker); });
      } else {
        overview.classList.add("empty");
      }
      shell.append(body, overview);
      return shell;
    }

    _overviewMarker(classes, index, total, label, targetRowId) {
      var trace = (classes || []).filter(isTraceGroup);
      if (!trace.length || !total) return null;
      var markerClasses = ["overview-marker", "trace-region"].concat(this._overviewMarkerKinds(classes || [])).concat(classes || []);
      var marker = el("button", markerClasses.join(" "));
      this._bindTraceGroups(marker, classes || []);
      marker.type = "button";
      marker.title = label || "evidence match";
      marker.dataset.traceLabel = label || "evidence match";
      if (targetRowId) marker.dataset.targetRow = targetRowId;
      marker.style.top = (((index + 0.5) / total) * 100).toFixed(3) + "%";
      return marker;
    }

    _overviewMarkerKinds(classes) {
      var out = [];
      if ((classes || []).some(function (name) { return name.indexOf("exact-c-") === 0; })) out.push("overview-exact");
      if ((classes || []).some(function (name) { return name.indexOf("generated-c-") === 0; }) || classes.indexOf("origin-generated") >= 0) out.push("overview-generated");
      if ((classes || []).some(function (name) { return name.indexOf("prepared-c-") === 0; })) out.push("overview-prepared");
      if ((classes || []).some(function (name) { return name.indexOf("context-c-") === 0; }) || classes.indexOf("origin-contextual") >= 0) out.push("overview-context");
      if ((classes || []).some(function (name) { return name.indexOf("structural-c-") === 0; }) || classes.indexOf("origin-structural") >= 0) out.push("overview-boundary");
      return out;
    }

    _rowId(prefix, index) {
      return "trace-row-" + String(prefix).replace(/[^a-zA-Z0-9_-]/g, "-") + "-" + index;
    }

    _bindTraceGroups(node, classes) {
      var groups = (classes || []).filter(isTraceGroup);
      node.__traceClasses = groups;
      if (groups.length) node.dataset.traceGroups = groups.join(" ");
    }

    _isBoundaryRow(rowData) {
      return ((rowData && rowData.meta) || "").indexOf("block") >= 0;
    }

    _isBoundaryKind(kind) {
      return /^(file_header|function_header|block_header|boundary_marker|derived_bytecode_block_header)$/.test(kind || "");
    }

    _rowBoundaryLabel(panelData, rowData, index) {
      var kind = rowData && rowData.kind;
      if (kind === "file_header" || kind === "function_header" || kind === "block_header" || kind === "boundary_marker" || kind === "derived_bytecode_block_header") {
        return this._typedBoundaryLabel(panelData, rowData, index);
      }
      return this._boundaryLabel(panelData, rowData, index);
    }

    _typedBoundaryLabel(panelData, rowData, index) {
      var kind = rowData && rowData.kind;
      var meta = (rowData && rowData.meta) || "";
      var label = (rowData && (rowData.label || rowData.compact_text || rowData.text)) || "";
      var role = this._loopRole(meta);
      if (kind === "file_header") return label || "source file";
      if (kind === "function_header") return label || (panelData && panelData.title ? panelData.title + " function" : "function");
      if (kind === "boundary_marker") return label || "boundary";
      if (kind === "derived_bytecode_block_header") return label || "derived bytecode block";
      if (panelData && panelData.id === "bytecode") return label || "derived bytecode block";
      if (panelData && panelData.id === "mir") return "MIR block" + (role ? " · " + role : "") + (label ? " · " + label : "");
      if (panelData && panelData.id && panelData.id.indexOf("sonatina") === 0) return (panelData.title || "Sonatina") + " block" + (role ? " · " + role : "") + (label ? " · " + label : "");
      return (label || "block") + (role ? " · " + role : "");
    }

    _boundaryLabel(panelData, rowData, index) {
      if (!panelData) return null;
      var meta = (rowData && rowData.meta) || "";
      if (this._isBoundaryRow(rowData)) {
        var role = this._loopRole(meta);
        if (panelData.id.indexOf("sonatina") === 0) {
          return (panelData.title || "Sonatina") + " static CFG block" + (role ? " · " + role : "");
        }
        if (panelData.id === "mir") return "MIR static CFG block" + (role ? " · " + role : "");
        if (panelData.id === "loop") return "Static loop CFG block · " + meta;
        return "Static CFG block";
      }
      if (panelData.id !== "bytecode") return null;
      var text = (rowData && rowData.text) || "";
      if (index === 0 || text.indexOf("ir[0] ") === 0) return "derived bytecode block";
      if (/\bJUMPDEST\b/.test(text)) return "derived bytecode block at JUMPDEST";
      return null;
    }

    _loopRole(meta) {
      var match = String(meta || "").match(/loop [^·]+/);
      return match ? match[0].trim() : "";
    }

    _panelHead(title, summary) {
      var head = el("div", "panel-head");
      head.append(el("h2", "", title || "Panel"));
      if (summary) head.append(el("p", "", summary));
      return head;
    }

    _detailPanel() {
      var panel = el("section", "panel detail-panel");
      panel.append(this._panelHead("Selection Details", "Click a highlighted source, IR, loop, or bytecode row to inspect evidence paths, siblings, and gaps."));
      panel.append(el("div", "detail", ""));
      return panel;
    }

    _installInteractions() {
      if (this._interactionsInstalled) return;
      this._interactionsInstalled = true;
      var root = this.shadowRoot;
      root.addEventListener("mouseover", function (event) {
        var row = event.target.closest && event.target.closest(".trace-region");
        if (!row || (event.relatedTarget && row.contains(event.relatedTarget))) return;
        this._queueHover(traceClasses(row));
      }.bind(this));
      root.addEventListener("mouseout", function (event) {
        var row = event.target.closest && event.target.closest(".trace-region");
        if (!row || (event.relatedTarget && row.contains(event.relatedTarget))) return;
        this._queueHover([]);
      }.bind(this));
      root.addEventListener("click", function (event) {
        var jump = event.target.closest && event.target.closest("[data-pane-jump]");
        if (jump) {
          event.preventDefault();
          this._jumpSelectedRun(jump);
          return;
        }
        var row = event.target.closest && event.target.closest(".trace-region");
        if (!row) return;
        event.preventDefault();
        var marker = event.target.closest && event.target.closest(".overview-marker");
        if (marker) this._scrollToMarkerTarget(marker);
        var groups = traceClasses(row);
        this._selectedDisplayClasses = allClasses(row);
        this._selectedTraceLabel = row.dataset.traceLabel || "";
        this._select(groups);
        this._scrollPeerPanesToSelection(groups, row);
        this._pinHashForRow(row);
      }.bind(this));
      root.addEventListener("change", function (event) {
        var select = event.target.closest && event.target.closest(".representation-select");
        var mode = event.target.closest && event.target.closest(".display-mode-select");
        var scrollState = this._capturePaneScrollState();
        var switchedPaneIndex = null;
        if (select) {
          var index = Number(select.dataset.paneIndex || 0);
          this._paneChoices[index] = select.value;
          switchedPaneIndex = index;
        } else if (mode) {
          this._displayMode = mode.value || "compact";
        } else {
          return;
        }
        this._render({
          scrollState: scrollState,
          switchedPaneIndex: switchedPaneIndex,
        });
      }.bind(this));
    }

    _queueHover(groups) {
      this._pendingHover = (groups || []).slice();
      if (this._hoverFrame) return;
      var raf = window.requestAnimationFrame || function (fn) { return window.setTimeout(fn, 16); };
      this._hoverFrame = raf(function () {
        this._hoverFrame = 0;
        this._applyHover(this._pendingHover || []);
      }.bind(this));
    }

    _applyHover(groups) {
      if (this._sameGroups(this._hovered, groups)) return;
      this._hovered = groups.slice();
      this._syncStateStyles();
    }

    _sameGroups(left, right) {
      if ((left || []).length !== (right || []).length) return false;
      var seen = Object.create(null);
      (left || []).forEach(function (group) { seen[group] = (seen[group] || 0) + 1; });
      return (right || []).every(function (group) {
        if (!seen[group]) return false;
        seen[group] -= 1;
        return true;
      });
    }

    _select(groups) {
      this._selected = groups.slice();
      this._renderDetail(this._selected);
      this._syncStateStyles();
    }

    _handleHashChange() {
      var hash = window.location.hash || "";
      if (this._skipHashChange === hash) {
        this._skipHashChange = "";
        return;
      }
      this._scheduleHashNavigation();
    }

    _scheduleHashNavigation() {
      var token = (this._hashNavigationToken || 0) + 1;
      this._hashNavigationToken = token;
      if (this._hashNavigationFrame) {
        var cancel = window.cancelAnimationFrame || window.clearTimeout;
        cancel(this._hashNavigationFrame);
      }
      var raf = window.requestAnimationFrame || function (fn) { return window.setTimeout(fn, 16); };
      this._hashNavigationFrame = raf(function () {
        this._hashNavigationFrame = 0;
        raf(function () {
          if (this._hashNavigationToken !== token) return;
          this._navigateToHash({ behavior: "auto" });
        }.bind(this));
      }.bind(this));
    }

    _navigateToHash(options) {
      var params = this._hashParams(window.location.hash || "");
      var row = this._rowForHashParams(params);
      if (!row && params && params.node) {
        var switchedPaneIndex = this._ensureRepresentationVisibleForNode(params.node);
        if (switchedPaneIndex !== null) {
          this._render({
            scrollState: this._capturePaneScrollState(),
            switchedPaneIndex: switchedPaneIndex,
          });
          return true;
        }
      }
      if (!row) {
        if (!window.location.hash) {
          this._selected = [];
          this._selectedDisplayClasses = [];
          this._renderDetail([]);
          this._syncStateStyles();
        }
        return false;
      }
      var groups = traceClasses(row);
      this._selectedDisplayClasses = allClasses(row);
      this._select(groups);
      this._scrollRowIntoView(row, options);
      this._scrollPeerPanesToSelection(groups, row, options);
      return true;
    }

    _rowForHash(hash) {
      return this._rowForHashParams(this._hashParams(hash));
    }

    _rowForHashParams(params) {
      if (!params) return null;
      if (params.node) {
        var nodeClass = keyClass(params.node);
        var byNode = this.shadowRoot.querySelector(".trace-region." + this._escapeClass(nodeClass));
        if (byNode) return byNode;
      }
      if (params.source) {
        var bySource = this.shadowRoot.querySelector('[data-source-ref="' + this._escapeAttribute(params.source) + '"]');
        if (bySource) return bySource;
      }
      if (params.row) {
        return this.shadowRoot.getElementById(params.row);
      }
      return null;
    }

    _ensureRepresentationVisibleForNode(key) {
      var representation = this._representationForOriginKey(key);
      if (!representation) return null;
      this._paneChoices = this._paneChoices || this._defaultPaneChoices(this._representations());
      if (this._paneChoices.indexOf(representation) >= 0) return null;
      var preferred = representation === "bytecode" ? 2 : 1;
      if (representation === "source") preferred = 0;
      this._paneChoices[preferred] = representation;
      return preferred;
    }

    _representationForOriginKey(key) {
      var kind = String(key || "").split("\u001f")[0];
      if (kind === "bytecode.pc") return "bytecode";
      if (kind.indexOf("runtime.") === 0) return "mir";
      if (kind.indexOf("hir.") === 0) return "hir";
      if (kind.indexOf("sonatina.evm.prepared.") === 0) return "sonatina-prepared";
      if (kind.indexOf("sonatina.postopt.") === 0) return "sonatina-post";
      if (kind.indexOf("sonatina.preopt.") === 0) return "sonatina-pre";
      return null;
    }

    _pinHashForRow(row) {
      var hash = this._hashForRow(row);
      if (!hash || window.location.hash === hash) return;
      if (window.history && window.history.pushState) {
        window.history.pushState(null, "", hash);
      } else {
        this._skipHashChange = hash;
        window.location.hash = hash;
      }
    }

    _hashForRow(row) {
      if (!row || !row.dataset) return "";
      if (row.dataset.originKey) return "#node=" + encodeURIComponent(row.dataset.originKey);
      if (row.dataset.sourceRef) return "#source=" + encodeURIComponent(row.dataset.sourceRef);
      if (row.id) return "#row=" + encodeURIComponent(row.id);
      return "";
    }

    _hashParams(hash) {
      hash = String(hash || "").replace(/^#/, "");
      if (!hash) return null;
      var params = new URLSearchParams(hash.indexOf("=") >= 0 ? hash : "node=" + hash);
      var out = Object.create(null);
      ["node", "source", "row"].forEach(function (key) {
        var value = params.get(key);
        if (value) out[key] = value;
      });
      return out;
    }

    _escapeAttribute(value) {
      return String(value).replace(/["\\]/g, "\\$&");
    }

    _capturePaneScrollState() {
      var state = Object.create(null);
      this.shadowRoot.querySelectorAll(".workbench-pane").forEach(function (pane) {
        var index = pane.dataset.paneIndex;
        var shell = pane.querySelector(".listing-shell");
        var scroller = this._scrollContainer(shell);
        if (!index || !scroller) return;
        state[index] = {
          representation: this._paneChoices && this._paneChoices[Number(index)],
          scrollTop: scroller.scrollTop,
          activeRun: shell && shell.dataset.activeRun || "",
        };
      }, this);
      return state;
    }

    _restorePaneScroll(restore) {
      if (!restore || !restore.scrollState) return;
      var switchedPaneIndex = restore.switchedPaneIndex;
      this.shadowRoot.querySelectorAll(".workbench-pane").forEach(function (pane) {
        var index = pane.dataset.paneIndex;
        var shell = pane.querySelector(".listing-shell");
        var scroller = this._scrollContainer(shell);
        if (!index || !shell || !scroller) return;
        if (switchedPaneIndex !== null && Number(index) === Number(switchedPaneIndex)) {
          this._scrollPaneToFirstSelectedRun(pane);
          return;
        }
        var saved = restore.scrollState[index];
        if (!saved) return;
        scroller.scrollTop = saved.scrollTop || 0;
        if (saved.activeRun) shell.dataset.activeRun = saved.activeRun;
      }, this);
    }

    _scrollContainer(shell) {
      return shell && shell.querySelector(".source-lines,.rows");
    }

    _scrollPaneToFirstSelectedRun(pane, options) {
      if (!this._selected || !this._selected.length) return false;
      var shell = pane && pane.querySelector(".listing-shell");
      if (!shell) return false;
      var runs = this._matchingRuns(shell, this._selected);
      if (!runs.length) return false;
      shell.dataset.activeRun = "0";
      this._scrollRunIntoView(runs[0], 1, options);
      return true;
    }

    _scrollToMarkerTarget(marker) {
      var targetId = marker && marker.dataset && marker.dataset.targetRow;
      if (!targetId) return;
      var target = this.shadowRoot.getElementById(targetId);
      if (target) this._scrollRowIntoView(target);
    }

    _scrollPeerPanesToSelection(groups, origin, options) {
      if (!groups || !groups.length) return;
      var originShell = origin && origin.closest && origin.closest(".listing-shell");
      this.shadowRoot.querySelectorAll(".listing-shell").forEach(function (shell) {
        if (shell === originShell) return;
        var runs = this._matchingRuns(shell, groups);
        if (!runs.length) return;
        shell.dataset.activeRun = "0";
        this._scrollRunIntoView(runs[0], 1, options);
      }, this);
    }

    _jumpSelectedRun(button) {
      var pane = button.closest && button.closest(".workbench-pane");
      var shell = pane && pane.querySelector(".listing-shell");
      if (!shell || !this._selected || !this._selected.length) return;
      var runs = this._matchingRuns(shell, this._selected);
      if (!runs.length) return;
      var direction = button.dataset.paneJump === "prev" ? -1 : 1;
      var current = Number(shell.dataset.activeRun || "-1");
      if (!Number.isFinite(current) || current < 0 || current >= runs.length) {
        current = direction < 0 ? runs.length : -1;
      }
      var next = direction < 0 ? current - 1 : current + 1;
      if (next < 0) next = runs.length - 1;
      if (next >= runs.length) next = 0;
      shell.dataset.activeRun = String(next);
      this._scrollRunIntoView(runs[next], direction);
    }

    _matchingRuns(shell, groups) {
      groups = this._highlightGroups(groups);
      var wanted = Object.create(null);
      (groups || []).forEach(function (group) { wanted[group] = true; });
      var key = (groups || []).slice().sort().join("|");
      shell.__traceRunCache = shell.__traceRunCache || Object.create(null);
      if (shell.__traceRunCache[key]) return shell.__traceRunCache[key];
      var rows = this._traceRows(shell);
      var runs = [];
      var current = null;
      var lastMatchedIndex = -2;
      rows.forEach(function (row, index) {
        var matches = traceClasses(row).some(function (group) { return wanted[group]; });
        if (!matches) {
          current = null;
          return;
        }
        if (current && index === lastMatchedIndex + 1) {
          current.push(row);
        } else {
          current = [row];
          runs.push(current);
        }
        lastMatchedIndex = index;
      });
      shell.__traceRunCache[key] = runs;
      return runs;
    }

    _traceRows(shell) {
      if (!shell.__traceRows) {
        shell.__traceRows = Array.prototype.slice.call(shell.querySelectorAll(".source-line.trace-region,.trace-row.trace-region"));
      }
      return shell.__traceRows;
    }

    _scrollRunIntoView(run, direction, options) {
      if (!run || !run.length) return;
      var pane = run[0].closest && run[0].closest(".workbench-pane");
      var representation = pane && pane.dataset && pane.dataset.representationId;
      var target = direction < 0 ? run[run.length - 1] : run[0];
      if (representation !== "bytecode") {
        var boundary = run.filter(function (row) { return row.classList.contains("boundary-row"); })[0] || this._nearestSectionBoundary(run[0]);
        target = boundary || target;
      }
      this._scrollRowIntoView(target, options);
    }

    _nearestSectionBoundary(row) {
      var node = row && row.previousElementSibling;
      while (node) {
        if (node.classList && (node.classList.contains("boundary-marker") || node.classList.contains("source-section-separator"))) {
          return node;
        }
        node = node.previousElementSibling;
      }
      return null;
    }

    _scrollRowIntoView(row, options) {
      var scroller = row && row.closest && row.closest(".source-lines,.rows");
      if (!scroller) return;
      var scrollerRect = scroller.getBoundingClientRect();
      var rowRect = row.getBoundingClientRect();
      var top = scroller.scrollTop
        + (rowRect.top - scrollerRect.top)
        - Math.max(0, (scroller.clientHeight - rowRect.height) / 2);
      scroller.scrollTo({
        top: Math.max(0, top),
        behavior: (options && options.behavior) || "smooth",
      });
    }

    _syncStateStyles() {
      if (!this._stateStyle) return;
      var hover = this._selectorForGroups(this._highlightGroups(this._hovered), ".trace-region");
      var selected = this._selectorForGroups(this._highlightGroups(this._selected), ".trace-region");
      var activeMarkers = this._selectorForGroups(this._highlightGroups((this._hovered || []).concat(this._selected || [])), ".overview-marker");
      var rules = [];
      if (hover) {
        rules.push(hover + "{background:color-mix(in srgb, var(--trace-accent) 10%, transparent) !important;outline:1px solid color-mix(in srgb, var(--trace-accent) 45%, transparent);outline-offset:-1px;}");
      }
      if (selected) {
        rules.push(selected + "{background:color-mix(in srgb, var(--trace-accent) 18%, transparent) !important;color:var(--trace-text) !important;outline:2px solid var(--trace-accent);outline-offset:-2px;}");
      }
      if (activeMarkers) {
        rules.push(activeMarkers + "{left:-1px;width:12px;height:10px;outline:0;background:var(--trace-accent) !important;border:1px solid var(--trace-text);box-shadow:0 0 0 2px color-mix(in srgb, var(--trace-code-bg) 80%, transparent),0 0 14px color-mix(in srgb, var(--trace-accent) 55%, transparent);z-index:2;}");
      }
      this._stateStyle.textContent = rules.join("\n");
    }

    _highlightGroups(groups) {
      var exact = [];
      var generated = [];
      var prepared = [];
      var context = [];
      var structural = [];
      var fallback = [];
      var seen = Object.create(null);
      (groups || []).forEach(function (group) {
        if (!isTraceGroup(group) || seen[group]) return;
        seen[group] = true;
        if (group.indexOf("exact-c-") === 0) {
          exact.push(group);
        } else if (group.indexOf("generated-c-") === 0) {
          generated.push(group);
        } else if (group.indexOf("prepared-c-") === 0) {
          prepared.push(group);
        } else if (group.indexOf("context-c-") === 0) {
          context.push(group);
        } else if (group.indexOf("structural-c-") === 0) {
          structural.push(group);
        } else {
          fallback.push(group);
        }
      });
      if (exact.length) return exact.concat(generated);
      if (generated.length) return generated;
      if (prepared.length) return prepared;
      if (context.length) return context;
      if (structural.length) return structural;
      return fallback;
    }

    _selectorForGroups(groups, base) {
      var seen = Object.create(null);
      var classes = [];
      (groups || []).forEach(function (group) {
        if (!group || seen[group]) return;
        seen[group] = true;
        classes.push("." + this._escapeClass(group));
      }, this);
      return classes.length ? base + ":is(" + classes.join(",") + ")" : "";
    }

    _escapeClass(name) {
      if (window.CSS && window.CSS.escape) return window.CSS.escape(name);
      return String(name).replace(/[^a-zA-Z0-9_-]/g, function (ch) {
        return "\\" + ch;
      });
    }

    _renderDetail(groups) {
      var detail = this.shadowRoot.querySelector(".detail");
      if (!detail) return;
      detail.textContent = "";
      if (!groups || groups.length === 0) {
        var statusCard = this._selectionStatusCard([], this._selectedDisplayClasses || []);
        if (statusCard) detail.append(statusCard);
        detail.append(el("p", "muted", "No evidence path selected."));
        detail.append(el("p", "audit-note", this._provenanceCopy()));
        if (this._data.audit && this._displayMode === "debug") {
          detail.append(this._auditSummary());
        }
        return;
      }
      var closuresByClass = Object.create(null);
      (this._data.closures || []).forEach(function (closure) {
        closuresByClass[closure.class_name] = closure;
      });
      var auditByClass = this._auditByClass();
      var selected = groups.map(function (group) { return closuresByClass[group]; }).filter(Boolean);
      var statusCard = this._selectionStatusCard(groups, this._selectedDisplayClasses || []);
      if (statusCard) detail.append(statusCard);
      var spanGroups = this._spanGroupsForClasses(groups);
      if (spanGroups.length && this._displayMode !== "compact") {
        var spanBox = el("div", "span-group-card");
        spanBox.append(el("h3", "", "Source span group"));
        spanGroups.forEach(function (group) {
          var section = el("div", "span-group");
          section.append(el("p", "span-title", (group.source_text || "unknown source") + " · " + group.closures + " evidence paths"));
          section.append(el("p", "muted", group.closures_with_targets + " target-connected · " + group.source_only_closures + " source-only sibling(s)"));
          section.append(this._memberList("Target-connected", group.target_connected_members || []));
          section.append(this._memberList("Source-only siblings", group.source_only_members || []));
          spanBox.append(section);
        }, this);
        detail.append(spanBox);
      }
      groups.forEach(function (group) {
        var closure = closuresByClass[group];
        if (!closure) return;
        var audit = auditByClass[group] || {};
        var box = el("div", "closure-card");
        box.classList.add.apply(box.classList, this._auditClasses([group]));
        box.append(el("h3", "", this._compactSelectionTitle(closure, audit)));
        box.append(this._auditHeader(audit, closure));
        if (closure.counts) {
          box.append(this._phaseRail(closure.counts, audit.highest_phase_reached));
        }
        if (closure.gap) {
          box.append(el("p", "gap", closure.gap));
        }
        (audit.notes || []).forEach(function (note) {
          box.append(el("p", "audit-note", note));
        });
        if (closure.traversal && this._displayMode === "debug") {
          box.append(el("p", "traversal", "connected-region walk: " + closure.traversal.mode + " · truncated=" + closure.traversal.truncated + " · skipped hubs=" + (closure.traversal.skipped_hubs || []).length));
        }
        if (this._displayMode === "debug") {
          (closure.edges || []).forEach(function (edge) {
            var traversal = String(edge.traversal_class || "unknown");
            var row = el("div", "edge edge-" + traversal.replace(/_/g, "-") + (edge.generated_work ? " edge-generated" : ""));
            row.append(
              el("span", "edge-label", edge.label),
              el("span", "edge-text", edge.from + " -> " + edge.to),
              el("span", "edge-class", traversal.replace(/_/g, " ") + (edge.generated_work ? " · generated" : ""))
            );
            box.append(row);
          });
        }
        if ((closure.source_spans || []).length && this._displayMode !== "compact") {
          box.append(el("h4", "", "Source spans"));
          closure.source_spans.forEach(function (span) {
            box.append(el("div", "span-line", span.lines + " " + span.origin));
          });
        }
        detail.append(box);
      }, this);
      if (selected.length > 1) {
        detail.insertBefore(el("p", "selection-note", selected.length + " evidence paths shown as a grouped view."), detail.firstChild);
      }
    }

    _auditSummary() {
      var audit = this._data.audit || {};
      var box = el("div", "audit-summary");
      box.append(el("h3", "", "Audit summary"));
      box.append(el("p", "muted", (audit.total_closures || 0) + " evidence paths · " + (audit.suspicious_closures || 0) + " need evidence · " + ((audit.span_groups && audit.span_groups.mixed_connectivity_groups) || 0) + " mixed source spans"));
      var counts = audit.primary_counts || {};
      Object.keys(counts).forEach(function (name) {
        var row = el("div", "audit-count");
        row.append(el("span", "", name), el("b", "", counts[name]));
        box.append(row);
      });
      return box;
    }

    _auditByClass() {
      if (this._auditByClassCache) return this._auditByClassCache;
      var byClass = Object.create(null);
      var audit = this._data.audit || {};
      (audit.closures || []).forEach(function (entry) {
        byClass[entry.class_name] = entry;
      });
      this._auditByClassCache = byClass;
      return byClass;
    }

    _auditForClasses(classes) {
      var byClass = this._auditByClass();
      return (classes || []).map(function (name) { return byClass[name]; }).filter(Boolean);
    }

    _selectionStatusCard(groups, displayClasses) {
      var entries = this._auditForClasses(groups);
      var status = this._displayStatus(entries, this._railStatus(displayClasses));
      if (!status) return null;
      var box = el("div", "status-card status-" + status.kind);
      box.append(el("h3", "", this._selectedTraceLabel || "Selected row evidence"));
      box.append(el("p", "status-line", status.label));
      box.append(el("p", "muted", this._statusExplanation(status.kind, status.label)));
      var reached = this._reachedSummary(entries);
      if (reached) box.append(reached);
      box.append(this._railLegend());
      return box;
    }

    _reachedSummary(entries) {
      if (!entries || !entries.length) return null;
      var counts = {
        hir: 0,
        mir: 0,
        sonatina_pre: 0,
        sonatina_post: 0,
        sonatina_prepared: 0,
        bytecode: 0,
      };
      entries.forEach(function (entry) {
        Object.keys(counts).forEach(function (key) {
          counts[key] += (entry.counts && entry.counts[key]) || 0;
        });
      });
      var rows = [
        ["Source/HIR", counts.hir],
        ["MIR", counts.mir],
        ["Sonatina pre-opt", counts.sonatina_pre],
        ["Optimized Sonatina", counts.sonatina_post],
        ["EVM prepared", counts.sonatina_prepared],
        ["Bytecode", counts.bytecode],
      ];
      var box = el("div", "reached-summary");
      box.append(el("h4", "", "Reached"));
      rows.forEach(function (row) {
        var item = el("span", "reach-chip" + (row[1] > 0 ? " reached" : ""));
        item.append(el("b", "", row[0]), el("em", "", row[1] > 0 ? "yes" : "no"));
        box.append(item);
      });
      var seam = el("p", "muted", this._provenanceCopy());
      box.append(seam);
      return box;
    }

    _statusExplanation(kind, label) {
      if (kind === "ok") return "Solid highlights use validated exact attribution or snapshot-alias continuity.";
      if (kind === "generated") return "Generated highlights show compiler-created synthetic work. They are useful explanation paths, but they do not count as exact source ownership.";
      if (kind === "explained") return "The missing direct bytecode link is explained by optimizer evidence such as elision, rewrite, creation, or snapshot-join facts.";
      if (kind === "context") return "Context highlights are navigation or cause context. They are intentionally separate from exact attribution.";
      if (kind === "structural") return "Boundary highlights show containment or derived block context, not provenance.";
      if (label === "missing link") return "This row reaches optimized compiler IR, but an exact downstream lineage edge is missing.";
      if (label === "MIR-only") return "This source/MIR region has precise MIR evidence but no Sonatina or bytecode lowering evidence yet.";
      if (label === "preopt-only") return "This region reaches Sonatina pre-opt but lacks a post-opt or bytecode explanation.";
      if (label === "prepared-linked") return "This final bytecode is linked to EVM prepared/codegen identity, but no optimized-Sonatina/source lineage is attached to that prepared instruction.";
      if (label === "library source") return "This row is attributed to std/core or other non-input source, not to the audited input file.";
      if (label === "compiler control") return "This row is compiler/runtime control-flow work without a direct user-source span.";
      if (label === "missing source") return "This row reaches compiler or bytecode artifacts without a direct source span. It may be generated or needs a better source edge.";
      return "This status is derived from the shared trace-query classifier and audit report, not from browser-side edge traversal.";
    }

    _railLegend() {
      var legend = el("div", "rail-legend");
      [
        ["ok", "exact"],
        ["generated", "generated"],
        ["context", "context"],
        ["structural", "boundary"],
      ].forEach(function (item) {
        legend.append(el("span", "legend-chip badge " + item[0], item[1]));
      });
      return legend;
    }

    _auditClasses(classes) {
      var entries = this._auditForClasses(classes);
      if (!entries.length) return [];
      var rank = -1;
      var primary = "unknown";
      entries.forEach(function (entry) {
        var next = this._severityRank(entry);
        if (next > rank) {
          rank = next;
          primary = entry.primary || "unknown";
        }
      }, this);
      var names = ["audit-" + primary.replace(/_/g, "-")];
      if (entries.some(function (entry) { return entry.suspicious; })) names.push("audit-needs-evidence");
      if (entries.some(function (entry) { return entry.primary && entry.primary.indexOf("good_") === 0; })) names.push("audit-good");
      return names;
    }

    _severityRank(entry) {
      if (!entry) return 0;
      if (entry.primary === "truncated_unknown" || entry.primary === "missing_source_unexplained") return 5;
      if (entry.primary === "lowered_no_target_unexplained" || entry.primary === "preopt_elision_gap") return 4;
      if (entry.primary === "optimized_attribution_gap") return 3;
      if (entry.primary === "optimizer_explained") return 0;
      if (entry.primary === "source_span_sibling_unlowered" || entry.primary === "source_only_expected") return 1;
      if (entry.primary && entry.primary.indexOf("good_") === 0) return 0;
      return entry.suspicious ? 4 : 1;
    }

    _badges(rowOrClasses) {
      var rowStatus = this._rowDisplayStatus(rowOrClasses);
      var classes = Array.isArray(rowOrClasses) ? rowOrClasses : ((rowOrClasses && rowOrClasses.classes) || []);
      var entries = this._auditForClasses(classes);
      var wrap = el("span", "badges");
      var status = rowStatus || this._displayStatus(entries, this._railStatus(classes));
      if (!status) return wrap;
      wrap.append(el("span", "badge " + status.kind, status.label));
      return wrap;
    }

    _rowDisplayStatus(rowData) {
      if (!rowData || Array.isArray(rowData) || !rowData.display_status) return null;
      var kind = String(rowData.display_status || "");
      if (kind === "exact") return { kind: "ok", label: "exact" };
      if (kind === "generated") return { kind: "generated", label: "generated" };
      if (kind === "generated_downstream") return { kind: "generated", label: "generated downstream" };
      if (kind === "context") return { kind: "context", label: "context" };
      if (kind === "prepared_linked") return { kind: "context", label: "prepared-linked" };
      if (kind === "missing_optimized_to_prepared") return { kind: "warn", label: "missing link" };
      if (kind === "missing_downstream_lineage") return { kind: "warn", label: "missing downstream" };
      if (kind === "source_only") return { kind: "context", label: "source-only" };
      if (kind === "compiler_generated") return { kind: "generated", label: "compiler-generated" };
      if (kind === "unmapped") return { kind: "warn", label: "unmapped" };
      if (kind === "ambiguous") return { kind: "warn", label: "ambiguous" };
      if (kind === "invalid") return { kind: "warn", label: "invalid" };
      return null;
    }

    _railStatus(classes) {
      classes = classes || [];
      if (classes.some(function (name) { return name.indexOf("exact-c-") === 0; })) return { kind: "ok", label: "exact" };
      if (classes.indexOf("origin-generated") >= 0) return { kind: "generated", label: "generated" };
      if (classes.some(function (name) { return name.indexOf("generated-c-") === 0; })) return { kind: "generated", label: "generated downstream" };
      if (classes.some(function (name) { return name.indexOf("prepared-c-") === 0; })) return { kind: "context", label: "prepared-linked" };
      if (classes.some(function (name) { return name.indexOf("context-c-") === 0; })) return { kind: "context", label: "context" };
      if (classes.indexOf("origin-contextual") >= 0) return { kind: "context", label: "context" };
      if (classes.some(function (name) { return name.indexOf("structural-c-") === 0; })) return { kind: "structural", label: "boundary" };
      if (classes.indexOf("origin-structural") >= 0) return { kind: "structural", label: "boundary" };
      return null;
    }

    _displayStatus(entries, railStatus) {
      if (railStatus) return railStatus;
      if (!entries || !entries.length) return null;
      var top = entries.slice().sort(function (a, b) {
        return this._severityRank(b) - this._severityRank(a);
      }.bind(this))[0];
      var primary = top.primary || "unknown";
      if (primary.indexOf("good_") === 0) return { kind: "ok", label: "exact" };
      if (primary === "optimizer_explained") return { kind: "explained", label: "explained" };
      if (primary === "optimized_attribution_gap") return { kind: "warn", label: "missing link" };
      if (primary === "lowered_no_target_unexplained") return { kind: "warn", label: "MIR-only" };
      if (primary === "preopt_elision_gap") return { kind: "warn", label: "preopt-only" };
      if (primary === "expected_synthetic") return { kind: "generated", label: "compiler-generated" };
      if (primary === "prepared_only") return { kind: "context", label: "prepared-linked" };
      if (primary === "source_span_sibling_unlowered" || primary === "source_only_expected") return { kind: "context", label: "source-only" };
      if (primary === "missing_source_unexplained") {
        if (entries.some(function (entry) { return (entry.symptoms || []).indexOf("foreign_source") >= 0; })) {
          return { kind: "context", label: "library source" };
        }
        if (top.counts && top.counts.hir === 0 && top.counts.mir > 0) {
          return { kind: "context", label: "compiler control" };
        }
        return { kind: "warn", label: "missing source" };
      }
      if (primary === "unclassified") return { kind: "warn", label: "needs evidence" };
      return { kind: top.suspicious ? "warn" : "context", label: this._shortPrimary(primary) };
    }

    _shortPrimary(primary) {
      return (primary || "unknown")
        .replace("optimized_attribution_gap", "missing link")
        .replace("source_span_sibling_unlowered", "span sibling")
        .replace("source_only_expected", "source only")
        .replace("optimizer_explained", "explained")
        .replace("good_many_to_many", "many-to-many")
        .replace("good_exact", "exact")
        .replace("missing_source_unexplained", "missing source")
        .replace("lowered_no_target_unexplained", "MIR-only")
        .replace("preopt_elision_gap", "preopt-only")
        .replace("prepared_only", "prepared-linked")
        .replace(/_/g, " ");
    }

    _auditHeader(audit, closure) {
      var head = el("div", "audit-header");
      head.append(el("span", "badge phase", this._phaseLabel(audit.highest_phase_reached || "unknown")));
      var status = this._displayStatus(audit && audit.primary ? [audit] : [], null);
      if (status) head.append(el("span", "badge " + status.kind, status.label));
      if (this._displayMode !== "compact") {
        (audit.symptoms || []).forEach(function (symptom) {
          head.append(el("span", "badge symptom", symptom.replace(/_/g, " ")));
        });
      }
      if (closure && closure.root_key && this._displayMode === "debug") {
        var key = el("code", "root-key", closure.root_key);
        key.title = closure.root_key;
        head.append(key);
      }
      return head;
    }

    _phaseRail(counts, highest) {
      var phases = [
        ["hir", "HIR", counts.hir],
        ["mir", "MIR", counts.mir],
        ["sonatina_pre", "Pre-opt", counts.sonatina_pre],
        ["sonatina_post", "Optimized", counts.sonatina_post],
        ["sonatina_prepared", "Prepared", counts.sonatina_prepared],
        ["bytecode", "Bytecode", counts.bytecode],
      ];
      var rail = el("div", "phase-rail");
      phases.forEach(function (phase) {
        var chip = el("span", "phase-chip", phase[1] + " " + phase[2]);
        if (phase[2] > 0) chip.classList.add("reached");
        if (phase[0] === highest) chip.classList.add("highest");
        rail.append(chip);
      });
      return rail;
    }

    _phaseLabel(phase) {
      var labels = {
        hir: "HIR",
        mir: "MIR",
        sonatina_pre: "Sonatina pre-opt",
        sonatina_post: "Optimized Sonatina",
        sonatina_prepared: "EVM prepared",
        bytecode: "Bytecode",
      };
      return labels[phase] || String(phase || "unknown").replace(/_/g, " ");
    }

    _compactSelectionTitle(closure, audit) {
      if (audit && audit.source_spans && audit.source_spans.length) {
        return audit.source_spans[0].replace(/ .*$/, "");
      }
      if (!closure) return "Selected evidence";
      return this._compactLabel(closure.label || closure.root_key || "Selected evidence");
    }

    _compactLabel(label) {
      var value = String(label || "");
      value = value.replace(/.*bytecode\\.pc[^:]*:pc:/, "PC ");
      value = value.replace(/.*sonatina\\.postopt\\.inst.*inst:/, "%");
      value = value.replace(/.*sonatina\\.evm\\.prepared\\.inst.*inst:/, "%");
      value = value.replace(/.*runtime\\.stmt.*block:/, "MIR bb");
      value = value.replace(/.*hir\\.expr.*expr:/, "HIR expr ");
      if (value.length > 96) value = value.slice(0, 93) + "...";
      return value;
    }

    _spanGroupsForClasses(classes) {
      var wanted = Object.create(null);
      (classes || []).forEach(function (name) { wanted[name] = true; });
      var audit = this._data.audit || {};
      return (audit.span_group_details || []).filter(function (group) {
        return (group.target_connected_members || []).concat(group.source_only_members || []).some(function (member) {
          return wanted[member.class_name];
        });
      });
    }

    _memberList(title, members) {
      var box = el("div", "member-list");
      box.append(el("h4", "", title));
      if (!members.length) {
        box.append(el("p", "muted", "none"));
        return box;
      }
      members.forEach(function (member) {
        var row = el("button", "member-row trace-region " + member.class_name + " " + this._auditClasses([member.class_name]).join(" "));
        this._bindTraceGroups(row, [member.class_name]);
        row.type = "button";
        row.append(
          el("span", "member-class", member.class_name),
          el("span", "member-phase", this._phaseLabel(member.highest_phase_reached)),
          el("span", "member-label", member.label)
        );
        box.append(row);
      }, this);
      return box;
    }

    _style() {
      return `
:host {
  --trace-bg: #1e1e2e;
  --trace-panel: #181825;
  --trace-elevated: #242438;
  --trace-text: #cdd6f4;
  --trace-muted: #a6adc8;
  --trace-accent: #89b4fa;
  --trace-border: #313244;
  --trace-code-bg: #11111b;
  --trace-code-text: #cdd6f4;
  --trace-line: #6c7086;
  --trace-pass: #a6e3a1;
  --trace-warn: #f9e2af;
  --trace-danger: #fab387;
  display:block;
  color:var(--trace-text);
  background:var(--trace-bg);
  font:12px/1.34 var(--fe-code-font, "JetBrains Mono", "Fira Code", ui-monospace, monospace);
}
:host-context([data-theme="light"]) {
  --trace-bg: #ffffff;
  --trace-panel: #f8fafc;
  --trace-elevated: #eff1f5;
  --trace-text: #1e293b;
  --trace-muted: #64748b;
  --trace-accent: #4f46e5;
  --trace-border: #e2e8f0;
  --trace-code-bg: #eff1f5;
  --trace-code-text: #4c4f69;
  --trace-line: #9ca0b0;
  --trace-pass: #16a34a;
  --trace-warn: #b08800;
  --trace-danger: #ea580c;
}
* { box-sizing:border-box; }
.trace-page { min-height:100vh; background:var(--trace-bg); }
.trace-header { display:grid; grid-template-columns:auto minmax(0,1fr) auto; gap:12px; align-items:end; padding:8px 20px 6px; border-bottom:1px solid var(--trace-border); }
h1 { margin:0; color:var(--trace-text); font:700 15px/1.1 ui-sans-serif, system-ui, sans-serif; }
.subtitle { max-width:980px; margin:0; color:var(--trace-muted); font:11px/1.3 ui-sans-serif, system-ui, sans-serif; text-align:right; }
.header-controls { display:flex; justify-content:flex-end; }
.display-mode-select { border:1px solid var(--trace-border); border-radius:6px; background:var(--trace-panel); color:var(--trace-text); padding:3px 6px; font:inherit; font-size:11px; }
.cards { display:grid; grid-template-columns:repeat(6,minmax(0,1fr)); gap:7px; padding:8px 20px; }
.card,.panel { background:var(--trace-panel); border:1px solid var(--trace-border); border-radius:8px; box-shadow:none; }
.card { padding:6px 8px; min-width:0; }
.card span { display:block; color:var(--trace-muted); font-size:9px; text-transform:uppercase; letter-spacing:.06em; }
.card b { display:block; margin-top:2px; color:var(--trace-accent); overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.analysis { padding:10px; border:1px solid var(--trace-border); border-radius:8px; background:var(--trace-panel); box-shadow:none; min-width:0; }
.analysis-head { display:flex; justify-content:space-between; gap:18px; align-items:end; margin-bottom:12px; }
.analysis-head h2 { margin:0; color:var(--trace-text); font-size:13px; text-transform:uppercase; letter-spacing:.1em; }
.analysis-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:10px; }
.analysis-card,.bloat-card { padding:12px; border:1px solid var(--trace-border); border-radius:8px; background:var(--trace-bg); }
.analysis-card.status-pass { border-left:3px solid var(--trace-pass); }
.analysis-card.status-warning { border-left:3px solid var(--trace-warn); }
.analysis-card.status-inconclusive { border-left:3px solid var(--trace-accent); }
.analysis-card h3,.bloat-card h3 { margin:0 0 6px; color:var(--trace-text); font-size:14px; }
.analysis-status { margin:0 0 6px; color:var(--trace-accent); text-transform:uppercase; letter-spacing:.07em; font-size:11px; }
.bloat-card { margin-top:10px; }
.bloat-row { display:flex; justify-content:space-between; gap:12px; padding:6px 0; border-top:1px solid var(--trace-border); color:var(--trace-muted); }
.bloat-kind { color:var(--trace-text); }
.bloat-count { color:var(--trace-muted); white-space:nowrap; }
.workspace { display:block; padding:0 20px 10px; }
.pane-deck { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:8px; min-width:0; }
.bottom-deck { display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); gap:10px; padding:0 20px 16px; align-items:start; }
.workbench-pane { min-width:0; }
.workbench-head { display:grid; grid-template-columns:minmax(0,1fr) auto auto; gap:6px; align-items:center; }
.workbench-head p { grid-column:1 / -1; }
.representation-select { max-width:120px; border:1px solid var(--trace-border); border-radius:6px; background:var(--trace-bg); color:var(--trace-text); padding:3px 5px; font:inherit; font-size:10px; }
.jump-controls { display:flex; gap:3px; justify-content:flex-end; }
.jump-button { border:1px solid var(--trace-border); border-radius:999px; background:var(--trace-bg); color:var(--trace-muted); padding:3px 5px; font:inherit; font-size:9px; text-transform:uppercase; letter-spacing:.05em; cursor:pointer; }
.jump-button:hover { color:var(--trace-text); border-color:var(--trace-accent); background:color-mix(in srgb, var(--trace-accent) 10%, var(--trace-bg)); }
.panel { overflow:hidden; min-height:220px; }
.workbench-pane { background:var(--trace-code-bg); }
.detail-panel { min-width:0; }
.panel-head { padding:6px 8px; border-bottom:1px solid var(--trace-border); }
.panel-head h2 { margin:0; color:var(--trace-text); font-size:10px; text-transform:uppercase; letter-spacing:.08em; }
	.panel-head p { margin:3px 0 0; color:var(--trace-muted); font-size:10px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
	.listing-shell { display:grid; grid-template-columns:minmax(0,1fr) 10px; min-height:0; }
	.source-lines,.rows { max-height:68vh; overflow:auto; overscroll-behavior:contain; scrollbar-width:thin; scrollbar-color:color-mix(in srgb, var(--trace-accent) 45%, var(--trace-border)) var(--trace-code-bg); }
	.source-lines::-webkit-scrollbar,.rows::-webkit-scrollbar,.detail::-webkit-scrollbar { width:7px; height:7px; }
	.source-lines::-webkit-scrollbar-track,.rows::-webkit-scrollbar-track,.detail::-webkit-scrollbar-track { background:var(--trace-code-bg); }
	.source-lines::-webkit-scrollbar-thumb,.rows::-webkit-scrollbar-thumb,.detail::-webkit-scrollbar-thumb { background:color-mix(in srgb, var(--trace-accent) 45%, var(--trace-border)); border-radius:999px; border:2px solid var(--trace-code-bg); }
	.overview-rail { position:relative; width:10px; min-height:100%; border-left:1px solid var(--trace-border); background:color-mix(in srgb, var(--trace-code-bg) 82%, var(--trace-panel)); }
	.overview-rail.empty { opacity:.35; }
	.overview-marker { position:absolute; left:1px; width:8px; height:5px; min-height:5px; padding:0; border:0; border-radius:999px; transform:translateY(-50%); background:color-mix(in srgb, var(--trace-accent) 82%, var(--trace-text)); box-shadow:0 0 0 1px color-mix(in srgb, var(--trace-code-bg) 80%, transparent),0 0 8px color-mix(in srgb, var(--trace-accent) 28%, transparent); cursor:pointer; }
	.overview-marker.overview-exact { background:var(--trace-accent); height:6px; }
	.overview-marker.overview-generated { background:transparent; border:1px dashed var(--trace-warn); height:7px; }
	.overview-marker.overview-prepared { background:color-mix(in srgb, var(--trace-pass) 75%, var(--trace-accent)); height:6px; }
	.overview-marker.overview-context { background:var(--trace-muted); opacity:.8; }
	.overview-marker.overview-boundary { background:transparent; border:1px solid var(--trace-muted); }
	.overview-marker.audit-needs-evidence { background:var(--trace-warn); }
	.overview-marker.audit-good { background:var(--trace-pass); }
	.overview-marker.origin-generated { height:7px; background:transparent; border:1px dashed var(--trace-warn); }
	.overview-marker.origin-contextual { background:var(--trace-muted); opacity:.82; }
	.overview-marker.origin-structural { background:transparent; border:1px solid var(--trace-muted); }
	.detail { max-height:420px; overflow:auto; }
.empty-pane { margin:12px; color:var(--trace-muted); }
.source-line { display:grid; grid-template-columns:30px minmax(0,1fr) auto; gap:6px; padding:0 6px; white-space:pre; cursor:pointer; align-items:center; }
.source-section-separator { display:flex; gap:6px; align-items:baseline; justify-content:space-between; margin-top:6px; padding:5px 6px 4px 36px; border-top:1px solid var(--trace-border); color:var(--trace-muted); background:color-mix(in srgb, var(--trace-panel) 84%, var(--trace-code-bg)); font:600 9px/1.25 ui-sans-serif, system-ui, sans-serif; text-transform:uppercase; letter-spacing:.06em; }
.source-section-separator small { min-width:0; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; color:color-mix(in srgb, var(--trace-muted) 72%, var(--trace-code-text)); text-transform:none; letter-spacing:0; font-weight:500; }
.related-source-line { color:color-mix(in srgb, var(--trace-code-text) 88%, var(--trace-muted)); }
.source-line code { color:var(--trace-code-text); font:inherit; }
.ln { color:var(--trace-line); text-align:right; user-select:none; }
.trace-row { display:grid; grid-template-columns:minmax(42px,.18fr) minmax(46px,.22fr) minmax(90px,1fr) auto; width:100%; gap:4px; padding:4px 6px; border:0; border-bottom:1px solid var(--trace-border); background:transparent; color:var(--trace-code-text); text-align:left; font:inherit; cursor:pointer; align-items:center; }
.trace-row .row-text { padding-left:calc(var(--row-indent, 0) * 10px); }
.trace-row:hover,.source-line:hover { background:color-mix(in srgb, var(--trace-accent) 8%, transparent); }
.trace-row.unlinked-row { color:var(--trace-muted); cursor:default; }
.trace-row.unlinked-row:hover { background:transparent; }
.trace-row.origin-generated { background:color-mix(in srgb, var(--trace-warn) 6%, transparent); border-left:2px dashed color-mix(in srgb, var(--trace-warn) 60%, transparent); }
.trace-row.origin-generated .row-meta::after { content:"generated"; display:inline-block; margin-left:6px; padding:1px 5px; border:1px solid color-mix(in srgb, var(--trace-warn) 42%, var(--trace-border)); border-radius:999px; color:var(--trace-warn); font-size:9px; text-transform:uppercase; letter-spacing:.06em; vertical-align:middle; }
.trace-row.origin-contextual { color:var(--trace-muted); background:color-mix(in srgb, var(--trace-accent) 4%, transparent); }
.trace-row.origin-contextual .row-meta::after { content:"context"; display:inline-block; margin-left:6px; padding:1px 5px; border:1px solid var(--trace-border); border-radius:999px; color:var(--trace-muted); font-size:9px; text-transform:uppercase; letter-spacing:.06em; vertical-align:middle; }
.trace-row.origin-structural { box-shadow:inset 0 0 0 1px color-mix(in srgb, var(--trace-muted) 25%, transparent); }
.row-label { color:var(--trace-accent); overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.row-meta { color:color-mix(in srgb, var(--trace-accent) 80%, var(--trace-text)); overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.row-text { color:var(--trace-code-text); overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.row-kind-function-header,.row-kind-block-header,.row-kind-boundary-marker,.row-kind-derived-bytecode-block-header { background:color-mix(in srgb, var(--trace-accent) 5%, transparent); font-weight:650; }
.row-kind-function-header .row-text,.row-kind-block-header .row-text,.row-kind-boundary-marker .row-text,.row-kind-derived-bytecode-block-header .row-text { color:color-mix(in srgb, var(--trace-code-text) 86%, var(--trace-accent)); }
.row-kind-terminator .row-text { color:color-mix(in srgb, var(--trace-code-text) 82%, var(--trace-warn)); }
[class*="trace-c-"] { transition:background .16s ease-out, color .16s ease-out, outline-color .16s ease-out; }
.audit-good { box-shadow:inset 3px 0 var(--trace-pass); }
.audit-needs-evidence { box-shadow:inset 3px 0 var(--trace-danger); }
.audit-optimized-attribution-gap { box-shadow:inset 3px 0 var(--trace-warn); }
.audit-source-span-sibling-unlowered,.audit-source-only-expected { box-shadow:inset 3px 0 var(--trace-border); }
.detail { padding:14px 15px; }
.boundary-marker { position:sticky; top:0; z-index:1; padding:4px 9px; border-bottom:1px solid var(--trace-border); background:color-mix(in srgb, var(--trace-panel) 92%, var(--trace-accent)); color:var(--trace-muted); font:600 11px/1.3 ui-sans-serif, system-ui, sans-serif; text-transform:uppercase; letter-spacing:.06em; }
.boundary-row { background:color-mix(in srgb, var(--trace-accent) 6%, transparent); font-weight:600; }
.muted { color:var(--trace-muted); margin:0; }
.closure-card { padding:12px; border:1px solid var(--trace-border); border-radius:8px; background:var(--trace-bg); margin-bottom:12px; }
.closure-card.audit-good { border-left:3px solid var(--trace-pass); }
.closure-card.audit-needs-evidence { border-left:3px solid var(--trace-danger); }
.closure-card.audit-optimized-attribution-gap { border-left:3px solid var(--trace-warn); }
.closure-card.audit-source-span-sibling-unlowered,.closure-card.audit-source-only-expected { border-color:var(--trace-border); }
.closure-card h3 { margin:0 0 8px; color:var(--trace-text); font-size:14px; }
.closure-card h4 { margin:12px 0 6px; color:var(--trace-accent); font-size:12px; text-transform:uppercase; letter-spacing:.1em; }
.phase-counts { margin:0 0 8px; color:var(--trace-muted); font-size:12px; }
.gap { margin:8px 0; padding:8px 10px; border:1px solid var(--trace-warn); border-radius:8px; background:color-mix(in srgb, var(--trace-warn) 12%, var(--trace-bg)); color:var(--trace-text); }
.audit-note { margin:8px 0; padding:8px 10px; border:1px solid var(--trace-border); border-radius:8px; background:var(--trace-panel); color:var(--trace-muted); }
.traversal,.selection-note { color:var(--trace-muted); font-size:12px; margin:8px 0; }
.badges,.audit-header { display:flex; flex-wrap:wrap; gap:5px; align-items:center; justify-content:flex-end; min-width:0; }
.source-line > .badges,.trace-row > .badges { display:flex; }
.audit-header { justify-content:flex-start; margin:0 0 9px; }
.badge { display:inline-flex; align-items:center; max-width:150px; padding:2px 7px; border:1px solid var(--trace-border); border-radius:999px; color:var(--trace-muted); background:var(--trace-panel); font-size:10px; line-height:1.4; text-transform:uppercase; letter-spacing:.06em; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.badge.phase { color:var(--trace-accent); border-color:color-mix(in srgb, var(--trace-accent) 35%, var(--trace-border)); }
.badge.ok { color:var(--trace-pass); border-color:color-mix(in srgb, var(--trace-pass) 35%, var(--trace-border)); }
.badge.warn,.badge.symptom { color:var(--trace-warn); border-color:color-mix(in srgb, var(--trace-warn) 35%, var(--trace-border)); }
	.badge.explained { color:var(--trace-pass); border-color:color-mix(in srgb, var(--trace-pass) 35%, var(--trace-border)); border-style:dotted; }
	.badge.generated { color:var(--trace-warn); border-color:color-mix(in srgb, var(--trace-warn) 35%, var(--trace-border)); border-style:dashed; }
	.badge.context { color:var(--trace-muted); border-color:var(--trace-border); }
	.badge.structural { color:var(--trace-muted); border-color:var(--trace-border); }
.badge.group { color:var(--trace-muted); }
.root-key { display:block; max-width:100%; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; color:var(--trace-muted); font-size:11px; }
.phase-rail { display:grid; grid-template-columns:repeat(6,minmax(0,1fr)); gap:5px; margin:8px 0 10px; }
.phase-chip { padding:5px 6px; border:1px solid var(--trace-border); border-radius:6px; color:var(--trace-muted); background:var(--trace-panel); text-align:center; font-size:11px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.phase-chip.reached { color:var(--trace-text); background:color-mix(in srgb, var(--trace-accent) 8%, var(--trace-panel)); }
.phase-chip.highest { color:var(--trace-bg); background:var(--trace-accent); border-color:var(--trace-accent); }
.span-group-card,.audit-summary { padding:12px; border:1px solid var(--trace-border); border-radius:8px; background:var(--trace-bg); margin-bottom:12px; }
.span-group-card h3,.audit-summary h3 { margin:0 0 8px; color:var(--trace-text); font-size:13px; text-transform:uppercase; letter-spacing:.1em; }
.status-card { padding:12px; border:1px solid var(--trace-border); border-radius:8px; background:var(--trace-bg); margin-bottom:12px; }
.status-card.status-ok { border-left:3px solid var(--trace-pass); }
.status-card.status-explained { border-left:3px dotted var(--trace-pass); }
.status-card.status-generated { border-left:3px dashed var(--trace-warn); }
.status-card.status-context { border-left:3px solid var(--trace-muted); }
.status-card.status-structural { box-shadow:inset 0 0 0 1px color-mix(in srgb, var(--trace-muted) 30%, transparent); }
.status-card.status-warn { border-left:3px solid var(--trace-warn); }
.status-card h3 { margin:0 0 6px; color:var(--trace-text); font-size:13px; text-transform:uppercase; letter-spacing:.1em; }
.status-line { margin:0 0 6px; color:var(--trace-accent); text-transform:uppercase; letter-spacing:.07em; font-size:11px; }
.reached-summary { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:5px; margin:10px 0; }
.reached-summary h4 { grid-column:1 / -1; margin:0; color:var(--trace-muted); font-size:10px; text-transform:uppercase; letter-spacing:.08em; }
.reach-chip { display:flex; align-items:center; justify-content:space-between; gap:6px; padding:5px 6px; border:1px solid var(--trace-border); border-radius:6px; color:var(--trace-muted); background:var(--trace-panel); font-size:10px; min-width:0; }
.reach-chip b { overflow:hidden; text-overflow:ellipsis; white-space:nowrap; font-weight:600; }
.reach-chip em { font-style:normal; color:var(--trace-muted); }
.reach-chip.reached { color:var(--trace-text); border-color:color-mix(in srgb, var(--trace-accent) 35%, var(--trace-border)); background:color-mix(in srgb, var(--trace-accent) 8%, var(--trace-panel)); }
.reach-chip.reached em { color:var(--trace-accent); }
.reached-summary .muted { grid-column:1 / -1; font-size:10px; }
.missing-link-summary { display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:7px; margin:10px 0; }
.metric { padding:7px 8px; border:1px solid var(--trace-border); border-radius:8px; background:var(--trace-bg); min-width:0; }
.metric span { display:block; color:var(--trace-muted); font:600 9px/1.25 ui-sans-serif, system-ui, sans-serif; text-transform:uppercase; letter-spacing:.06em; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.metric b { display:block; margin-top:3px; color:var(--trace-accent); font-size:13px; }
.missing-targets h3 { margin:12px 0 6px; color:var(--trace-text); font-size:12px; text-transform:uppercase; letter-spacing:.08em; }
.rail-legend { display:flex; flex-wrap:wrap; gap:5px; margin-top:10px; }
.legend-chip { max-width:none; }
.span-group { padding:9px 0; border-top:1px solid var(--trace-border); }
.span-group:first-of-type { border-top:0; }
.span-title { margin:0 0 4px; color:var(--trace-accent); overflow-wrap:anywhere; }
.member-list h4 { margin:10px 0 5px; color:var(--trace-accent); font-size:11px; text-transform:uppercase; letter-spacing:.08em; }
.member-row { display:grid; grid-template-columns:72px 96px minmax(0,1fr); width:100%; gap:7px; padding:5px 7px; border:0; border-radius:6px; background:transparent; color:var(--trace-muted); text-align:left; font:inherit; cursor:pointer; }
.member-row:hover { background:color-mix(in srgb, var(--trace-accent) 8%, transparent); }
.member-class,.member-phase { color:var(--trace-accent); white-space:nowrap; }
.member-label { overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.audit-count { display:flex; justify-content:space-between; gap:10px; padding:4px 0; color:var(--trace-muted); border-top:1px solid var(--trace-border); }
.audit-count b { color:var(--trace-accent); }
.edge { display:grid; grid-template-columns:145px minmax(0,1fr) 120px; gap:8px; padding:4px 0; color:var(--trace-muted); }
.span-line { display:grid; grid-template-columns:145px 1fr; gap:8px; padding:4px 0; color:var(--trace-muted); }
.edge-label { color:var(--trace-accent); }
.edge-contextual .edge-label,.edge-synthetic .edge-label,.edge-generated .edge-label { color:var(--trace-warn); }
.edge-class { color:var(--trace-muted); font-size:10px; text-transform:uppercase; letter-spacing:.06em; text-align:right; }
.edge-text,.span-line { overflow-wrap:anywhere; }
.notes { margin:0; padding:0 32px 30px 52px; color:var(--trace-muted); }
.notes li { margin:6px 0; }
@media (max-width:1250px) { .cards { grid-template-columns:repeat(3,minmax(0,1fr)); } .pane-deck,.bottom-deck,.analysis-grid { grid-template-columns:1fr; } }
@media (max-width:980px) { .missing-link-summary { grid-template-columns:repeat(2,minmax(0,1fr)); } }
@media (max-width:760px) { .workspace,.bottom-deck,.cards,.pane-deck { padding-left:14px; padding-right:14px; } .cards,.analysis-grid,.pane-deck,.bottom-deck { grid-template-columns:1fr; } .analysis-head { display:block; } .trace-header { padding-left:14px; padding-right:14px; } .notes { padding-left:32px; padding-right:14px; } .workbench-head { grid-template-columns:1fr; } .representation-select { max-width:none; width:100%; } }
`;
    }
  }

  if (!customElements.get("fe-origin-trace")) {
    customElements.define("fe-origin-trace", FeOriginTrace);
  }
})();
