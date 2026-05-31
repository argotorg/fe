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
    return Array.prototype.filter.call(node.classList || [], function (name) {
      return name.indexOf("trace-c-") === 0;
    });
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
      this._paneChoices = null;
      this._displayMode = "compact";
      this._render();
    }

    _render() {
      var data = this._data;
      this.shadowRoot.textContent = "";
      this.shadowRoot.append(el("style", "", this._style()));

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
      this._restoreSelection();
      this._renderDetail(this._selected);
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
      this._card(cards, "bytecode", this._bytecodeSummary());
      this._card(cards, "source spans", data.source && data.source.confidence);
      return cards;
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
      var head = el("div", "panel-head workbench-head");
      var select = el("select", "representation-select");
      select.dataset.paneIndex = String(index);
      reps.forEach(function (rep) {
        var option = el("option", "", rep.title);
        option.value = rep.id;
        option.selected = rep.id === selected.id;
        select.append(option);
      });
      head.append(el("h2", "", selected ? selected.title : "Representation"), select);
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
      lines.forEach(function (line, index) {
        var classes = line.classes || [];
        var row = el("div", "source-line trace-region " + classes.concat(this._auditClasses(classes)).join(" "));
        row.dataset.traceLabel = "source line " + line.number;
        row.append(el("span", "ln", line.number), el("code", "", line.text), this._badges(classes));
        body.append(row);
        var marker = this._overviewMarker(classes, index, lines.length, "source line " + line.number);
        if (marker) markers.push(marker);
      }, this);
      if (!lines.length) {
        body.append(el("p", "empty-pane", "No source rows available."));
      }
      return this._listingShell(body, markers);
    }

    _panelBody(panelData) {
      var rows = el("div", "rows");
      var markers = [];
      var panelRows = panelData.rows || [];
      panelRows.forEach(function (rowData, index) {
        var boundaryLabel = this._boundaryLabel(panelData, rowData, index);
        if (boundaryLabel) {
          rows.append(el("div", "boundary-marker", boundaryLabel));
        }
        var classes = rowData.classes || [];
        var rowKind = this._isBoundaryRow(rowData) ? "boundary-row " : "";
        var row = el("button", "trace-row trace-region " + rowKind + (classes.length ? "" : "unlinked-row ") + classes.concat(this._auditClasses(classes)).join(" "));
        row.type = "button";
        if (rowData.key) {
          row.dataset.originKey = rowData.key;
          row.classList.add(keyClass(rowData.key));
        }
        row.dataset.traceLabel = rowData.label || rowData.text || rowData.key || "";
        row.append(
          el("span", "row-label", rowData.label),
          el("span", "row-meta", rowData.meta),
          el("span", "row-text", rowData.text),
          this._badges(classes)
        );
        rows.append(row);
        var marker = this._overviewMarker(classes, index, panelRows.length, rowData.label || rowData.text || rowData.key || "");
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

    _overviewMarker(classes, index, total, label) {
      var trace = (classes || []).filter(function (name) { return name.indexOf("trace-c-") === 0; });
      if (!trace.length || !total) return null;
      var markerClasses = ["overview-marker", "trace-region"].concat(classes || []);
      var marker = el("button", markerClasses.join(" "));
      marker.type = "button";
      marker.title = label || "evidence match";
      marker.dataset.traceLabel = label || "evidence match";
      marker.style.top = (((index + 0.5) / total) * 100).toFixed(3) + "%";
      return marker;
    }

    _isBoundaryRow(rowData) {
      return ((rowData && rowData.meta) || "").indexOf("block") >= 0;
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
      panel.append(this._panelHead("Selection Details", "Click a highlighted source, IR, loop, or bytecode row to inspect exact trace roots, siblings, and gaps."));
      panel.append(el("div", "detail", ""));
      return panel;
    }

    _installInteractions() {
      var root = this.shadowRoot;
      root.addEventListener("mouseover", function (event) {
        var row = event.target.closest && event.target.closest(".trace-region");
        if (!row || (event.relatedTarget && row.contains(event.relatedTarget))) return;
        this._setHover(traceClasses(row), true);
      }.bind(this));
      root.addEventListener("mouseout", function (event) {
        var row = event.target.closest && event.target.closest(".trace-region");
        if (!row || (event.relatedTarget && row.contains(event.relatedTarget))) return;
        this._setHover(traceClasses(row), false);
      }.bind(this));
      root.addEventListener("click", function (event) {
        var row = event.target.closest && event.target.closest(".trace-region");
        if (!row) return;
        var groups = traceClasses(row);
        this._select(groups);
      }.bind(this));
      root.addEventListener("change", function (event) {
        var select = event.target.closest && event.target.closest(".representation-select");
        var mode = event.target.closest && event.target.closest(".display-mode-select");
        if (select) {
          var index = Number(select.dataset.paneIndex || 0);
          this._paneChoices[index] = select.value;
        } else if (mode) {
          this._displayMode = mode.value || "compact";
        } else {
          return;
        }
        this._render();
      }.bind(this));
    }

    _setHover(groups, on) {
      this._forGroups(groups, function (node) {
        node.classList.toggle("trace-hover", on);
      });
    }

    _select(groups) {
      this._forGroups(this._selected, function (node) {
        node.classList.remove("trace-selected");
      });
      this._selected = groups.slice();
      this._forGroups(this._selected, function (node) {
        node.classList.add("trace-selected");
      });
      this._renderDetail(this._selected);
    }

    _restoreSelection() {
      this._forGroups(this._selected || [], function (node) {
        node.classList.add("trace-selected");
      });
    }

    _forGroups(groups, f) {
      var seen = Object.create(null);
      groups.forEach(function (group) {
        if (!group || seen[group]) return;
        seen[group] = true;
        this.shadowRoot.querySelectorAll("." + group).forEach(f);
      }, this);
    }

    _renderDetail(groups) {
      var detail = this.shadowRoot.querySelector(".detail");
      if (!detail) return;
      detail.textContent = "";
      if (!groups || groups.length === 0) {
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
      var spanGroups = this._spanGroupsForClasses(groups);
      if (spanGroups.length) {
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
      if (entry.primary === "source_span_sibling_unlowered" || entry.primary === "source_only_expected") return 1;
      if (entry.primary && entry.primary.indexOf("good_") === 0) return 0;
      return entry.suspicious ? 4 : 1;
    }

    _badges(classes) {
      var entries = this._auditForClasses(classes);
      var wrap = el("span", "badges");
      var status = this._displayStatus(entries, this._railStatus(classes));
      if (!status) return wrap;
      wrap.append(el("span", "badge " + status.kind, status.label));
      return wrap;
    }

    _railStatus(classes) {
      classes = classes || [];
      if (classes.indexOf("origin-generated") >= 0) return { kind: "generated", label: "generated" };
      if (classes.indexOf("origin-contextual") >= 0) return { kind: "context", label: "context" };
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
      if (primary === "optimized_attribution_gap") return { kind: "warn", label: "missing link" };
      if (primary === "lowered_no_target_unexplained" || primary === "preopt_elision_gap") return { kind: "warn", label: "missing downstream" };
      if (primary === "expected_synthetic") return { kind: "generated", label: "generated" };
      if (primary === "source_span_sibling_unlowered" || primary === "source_only_expected") return { kind: "context", label: "source-only" };
      if (primary === "missing_source_unexplained" || primary === "unclassified") return { kind: "warn", label: "unmapped" };
      return { kind: top.suspicious ? "warn" : "context", label: this._shortPrimary(primary) };
    }

    _shortPrimary(primary) {
      return (primary || "unknown")
        .replace("optimized_attribution_gap", "missing link")
        .replace("source_span_sibling_unlowered", "span sibling")
        .replace("source_only_expected", "source only")
        .replace("good_many_to_many", "many-to-many")
        .replace("good_exact", "exact")
        .replace("missing_source_unexplained", "missing source")
        .replace("lowered_no_target_unexplained", "lowering gap")
        .replace("preopt_elision_gap", "preopt gap")
        .replace(/_/g, " ");
    }

    _auditHeader(audit, closure) {
      var head = el("div", "audit-header");
      head.append(el("span", "badge phase", audit.highest_phase_reached || "unknown"));
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
        ["sonatina_pre", "PreOpt", counts.sonatina_pre],
        ["sonatina_post", "PostOpt", counts.sonatina_post],
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
        row.type = "button";
        row.append(
          el("span", "member-class", member.class_name),
          el("span", "member-phase", member.highest_phase_reached),
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
  font:13px/1.45 var(--fe-code-font, "JetBrains Mono", "Fira Code", ui-monospace, monospace);
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
.trace-header { display:grid; grid-template-columns:auto minmax(0,1fr) auto; gap:18px; align-items:end; padding:12px 32px 8px; border-bottom:1px solid var(--trace-border); }
h1 { margin:0; color:var(--trace-text); font:700 18px/1.15 ui-sans-serif, system-ui, sans-serif; }
.subtitle { max-width:980px; margin:0; color:var(--trace-muted); font:13px/1.35 ui-sans-serif, system-ui, sans-serif; text-align:right; }
.header-controls { display:flex; justify-content:flex-end; }
.display-mode-select { border:1px solid var(--trace-border); border-radius:6px; background:var(--trace-panel); color:var(--trace-text); padding:5px 7px; font:inherit; font-size:12px; }
.cards { display:grid; grid-template-columns:repeat(6,minmax(0,1fr)); gap:10px; padding:12px 32px; }
.card,.panel { background:var(--trace-panel); border:1px solid var(--trace-border); border-radius:8px; box-shadow:none; }
.card { padding:10px 12px; min-width:0; }
.card span { display:block; color:var(--trace-muted); font-size:11px; text-transform:uppercase; letter-spacing:.06em; }
.card b { display:block; margin-top:2px; color:var(--trace-accent); overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.analysis { padding:14px; border:1px solid var(--trace-border); border-radius:8px; background:var(--trace-panel); box-shadow:none; min-width:0; }
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
.workspace { display:block; padding:0 32px 14px; }
.pane-deck { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:14px; min-width:0; }
.bottom-deck { display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); gap:14px; padding:0 32px 24px; align-items:start; }
.workbench-pane { min-width:0; }
.workbench-head { display:grid; grid-template-columns:minmax(0,1fr) auto; gap:10px; align-items:center; }
.workbench-head p { grid-column:1 / -1; }
.representation-select { max-width:150px; border:1px solid var(--trace-border); border-radius:6px; background:var(--trace-bg); color:var(--trace-text); padding:5px 7px; font:inherit; font-size:12px; }
.panel { overflow:hidden; min-height:260px; }
.workbench-pane { background:var(--trace-code-bg); }
.detail-panel { min-width:0; }
.panel-head { padding:9px 10px; border-bottom:1px solid var(--trace-border); }
.panel-head h2 { margin:0; color:var(--trace-text); font-size:12px; text-transform:uppercase; letter-spacing:.1em; }
	.panel-head p { margin:4px 0 0; color:var(--trace-muted); font-size:11px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
	.listing-shell { display:grid; grid-template-columns:minmax(0,1fr) 10px; min-height:0; }
	.source-lines,.rows { max-height:62vh; overflow:auto; scrollbar-width:thin; scrollbar-color:color-mix(in srgb, var(--trace-accent) 45%, var(--trace-border)) var(--trace-code-bg); }
	.source-lines::-webkit-scrollbar,.rows::-webkit-scrollbar,.detail::-webkit-scrollbar { width:8px; height:8px; }
	.source-lines::-webkit-scrollbar-track,.rows::-webkit-scrollbar-track,.detail::-webkit-scrollbar-track { background:var(--trace-code-bg); }
	.source-lines::-webkit-scrollbar-thumb,.rows::-webkit-scrollbar-thumb,.detail::-webkit-scrollbar-thumb { background:color-mix(in srgb, var(--trace-accent) 45%, var(--trace-border)); border-radius:999px; border:2px solid var(--trace-code-bg); }
	.overview-rail { position:relative; width:10px; min-height:100%; border-left:1px solid var(--trace-border); background:color-mix(in srgb, var(--trace-code-bg) 82%, var(--trace-panel)); }
	.overview-rail.empty { opacity:.35; }
	.overview-marker { position:absolute; left:2px; width:6px; height:3px; min-height:3px; padding:0; border:0; border-radius:999px; transform:translateY(-50%); background:color-mix(in srgb, var(--trace-accent) 72%, var(--trace-text)); cursor:pointer; }
	.overview-marker.audit-needs-evidence { background:var(--trace-warn); }
	.overview-marker.audit-good { background:var(--trace-pass); }
	.overview-marker.origin-generated { height:5px; background:transparent; border:1px dashed var(--trace-warn); }
	.overview-marker.origin-contextual { background:var(--trace-muted); opacity:.7; }
	.overview-marker.origin-structural { background:transparent; border:1px solid var(--trace-muted); }
	.detail { max-height:420px; overflow:auto; }
.empty-pane { margin:12px; color:var(--trace-muted); }
.source-line { display:grid; grid-template-columns:38px minmax(0,1fr) auto; gap:8px; padding:0 10px; white-space:pre; cursor:pointer; align-items:center; }
.source-line code { color:var(--trace-code-text); font:inherit; }
.ln { color:var(--trace-line); text-align:right; user-select:none; }
.trace-row { display:grid; grid-template-columns:minmax(52px,.22fr) minmax(58px,.28fr) minmax(120px,1fr) auto; width:100%; gap:6px; padding:7px 9px; border:0; border-bottom:1px solid var(--trace-border); background:transparent; color:var(--trace-code-text); text-align:left; font:inherit; cursor:pointer; align-items:center; }
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
[class*="trace-c-"] { transition:background .16s ease-out, color .16s ease-out, outline-color .16s ease-out; }
	.trace-hover { background:color-mix(in srgb, var(--trace-accent) 10%, transparent) !important; outline:1px solid color-mix(in srgb, var(--trace-accent) 45%, transparent); outline-offset:-1px; }
	.trace-selected { background:color-mix(in srgb, var(--trace-accent) 18%, transparent) !important; color:var(--trace-text) !important; outline:2px solid var(--trace-accent); outline-offset:-2px; }
	.overview-marker.trace-hover,.overview-marker.trace-selected { left:0; width:10px; height:7px; outline:0; background:var(--trace-accent) !important; border:1px solid var(--trace-text); z-index:2; }
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
	.badge.generated { color:var(--trace-warn); border-color:color-mix(in srgb, var(--trace-warn) 35%, var(--trace-border)); border-style:dashed; }
	.badge.context { color:var(--trace-muted); border-color:var(--trace-border); }
	.badge.structural { color:var(--trace-muted); border-color:var(--trace-border); }
.badge.group { color:var(--trace-muted); }
.root-key { display:block; max-width:100%; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; color:var(--trace-muted); font-size:11px; }
.phase-rail { display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:5px; margin:8px 0 10px; }
.phase-chip { padding:5px 6px; border:1px solid var(--trace-border); border-radius:6px; color:var(--trace-muted); background:var(--trace-panel); text-align:center; font-size:11px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.phase-chip.reached { color:var(--trace-text); background:color-mix(in srgb, var(--trace-accent) 8%, var(--trace-panel)); }
.phase-chip.highest { color:var(--trace-bg); background:var(--trace-accent); border-color:var(--trace-accent); }
.span-group-card,.audit-summary { padding:12px; border:1px solid var(--trace-border); border-radius:8px; background:var(--trace-bg); margin-bottom:12px; }
.span-group-card h3,.audit-summary h3 { margin:0 0 8px; color:var(--trace-text); font-size:13px; text-transform:uppercase; letter-spacing:.1em; }
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
@media (max-width:760px) { .workspace,.bottom-deck,.cards,.pane-deck { padding-left:14px; padding-right:14px; } .cards,.analysis-grid,.pane-deck,.bottom-deck { grid-template-columns:1fr; } .analysis-head { display:block; } .trace-header { padding-left:14px; padding-right:14px; } .notes { padding-left:32px; padding-right:14px; } .workbench-head { grid-template-columns:1fr; } .representation-select { max-width:none; width:100%; } }
`;
    }
  }

  if (!customElements.get("fe-origin-trace")) {
    customElements.define("fe-origin-trace", FeOriginTrace);
  }
})();
