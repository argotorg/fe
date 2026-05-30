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
      this._render();
    }

    _render() {
      var data = this._data;
      this.shadowRoot.textContent = "";
      this.shadowRoot.append(el("style", "", this._style()));

      var page = el("div", "trace-page");
      var header = el("header", "trace-header");
      header.append(
        el("p", "eyebrow", "Fe origin trace"),
        el("h1", "", "Trace-through graph"),
        el(
          "p",
          "subtitle",
          "Derived browser view over compiler trace facts. Hover or click any highlighted region to select its connected trace region across source, HIR, MIR, Sonatina, and bytecode."
        )
      );
      page.append(header);

      var cards = el("section", "cards");
      this._card(cards, "data source", data.metadata && data.metadata.data_source);
      this._card(cards, "facts", data.counts && data.counts.facts);
      this._card(cards, "trace selections", (data.closures || []).length);
      this._card(cards, "suspicious", data.audit && data.audit.suspicious_closures);
      this._card(cards, "mixed spans", data.audit && data.audit.span_groups && data.audit.span_groups.mixed_connectivity_groups);
      this._card(cards, "source confidence", data.source && data.source.confidence);
      if (data.salsa) {
        this._card(cards, "salsa mode", data.salsa.mode);
        this._card(cards, "query execs", data.salsa.will_execute);
        this._card(cards, "memo reuse", data.salsa.memo_reuse);
        this._card(cards, "render ms", data.salsa.elapsed_ms);
      }
      page.append(cards);

      var workspace = el("main", "workspace");
      workspace.append(this._sourcePanel(data.source || {}));
      (data.panels || []).forEach(function (panel) {
        workspace.append(this._panel(panel));
      }, this);
      workspace.append(this._detailPanel());
      page.append(workspace);

      var notes = el("ul", "notes");
      (data.notes || []).forEach(function (note) {
        notes.append(el("li", "", note));
      });
      page.append(notes);
      this.shadowRoot.append(page);
      this._installInteractions();
      this._renderDetail(null);
    }

    _card(parent, label, value) {
      var card = el("div", "card");
      card.append(el("span", "", label), el("b", "", value == null ? "unknown" : value));
      parent.append(card);
    }

    _sourcePanel(source) {
      var panel = el("section", "panel source-panel");
      panel.append(this._panelHead("Source", source.display_name || "source"));
      var body = el("div", "source-lines");
      (source.lines || []).forEach(function (line) {
        var classes = line.classes || [];
        var row = el("div", "source-line trace-region " + classes.concat(this._auditClasses(classes)).join(" "));
        row.dataset.traceLabel = "source line " + line.number;
        row.append(el("span", "ln", line.number), el("code", "", line.text), this._badges(classes));
        body.append(row);
      }, this);
      panel.append(body);
      return panel;
    }

    _panel(panelData) {
      var panel = el("section", "panel");
      panel.append(this._panelHead(panelData.title, panelData.summary));
      var rows = el("div", "rows");
      (panelData.rows || []).forEach(function (rowData) {
        var classes = rowData.classes || [];
        var row = el("button", "trace-row trace-region " + classes.concat(this._auditClasses(classes)).join(" "));
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
      }, this);
      panel.append(rows);
      return panel;
    }

    _panelHead(title, summary) {
      var head = el("div", "panel-head");
      head.append(el("h2", "", title || "Panel"));
      if (summary) head.append(el("p", "", summary));
      return head;
    }

    _detailPanel() {
      var panel = el("section", "panel detail-panel");
      panel.append(this._panelHead("Origin Inspector", "Click any source, IR, loop, or bytecode region"));
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
        detail.append(el("p", "muted", "No trace selection selected."));
        if (this._data.audit) {
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
          section.append(el("p", "span-title", (group.source_text || "unknown source") + " · " + group.closures + " trace selections"));
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
        box.append(el("h3", "", closure.label));
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
        if (closure.traversal) {
          box.append(el("p", "traversal", "connected-region walk: " + closure.traversal.mode + " · truncated=" + closure.traversal.truncated + " · skipped hubs=" + (closure.traversal.skipped_hubs || []).length));
        }
        (closure.edges || []).forEach(function (edge) {
          var row = el("div", "edge");
          row.append(
            el("span", "edge-label", edge.label),
            el("span", "edge-text", edge.from + " -> " + edge.to)
          );
          box.append(row);
        });
        if ((closure.source_spans || []).length) {
          box.append(el("h4", "", "Source spans"));
          closure.source_spans.forEach(function (span) {
            box.append(el("div", "span-line", span.lines + " " + span.origin));
          });
        }
        detail.append(box);
      }, this);
      if (selected.length > 1) {
        detail.insertBefore(el("p", "selection-note", selected.length + " trace selections shown as a grouped view. Cards below remain exact per-origin trace roots."), detail.firstChild);
      }
    }

    _auditSummary() {
      var audit = this._data.audit || {};
      var box = el("div", "audit-summary");
      box.append(el("h3", "", "Audit summary"));
      box.append(el("p", "muted", (audit.total_closures || 0) + " trace selections · " + (audit.suspicious_closures || 0) + " suspicious · " + ((audit.span_groups && audit.span_groups.mixed_connectivity_groups) || 0) + " mixed source spans"));
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
      if (entries.some(function (entry) { return entry.suspicious; })) names.push("audit-suspicious");
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
      if (!entries.length) return wrap;
      var top = entries.slice().sort(function (a, b) {
        return this._severityRank(b) - this._severityRank(a);
      }.bind(this))[0];
      wrap.append(el("span", "badge phase", top.highest_phase_reached || "unknown"));
      wrap.append(el("span", "badge " + (top.suspicious ? "warn" : "ok"), this._shortPrimary(top.primary)));
      if (entries.length > 1) wrap.append(el("span", "badge group", entries.length + " roots"));
      return wrap;
    }

    _shortPrimary(primary) {
      return (primary || "unknown")
        .replace("optimized_attribution_gap", "opt gap")
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
      head.append(el("span", "badge " + (audit.suspicious ? "warn" : "ok"), this._shortPrimary(audit.primary)));
      (audit.symptoms || []).forEach(function (symptom) {
        head.append(el("span", "badge symptom", symptom.replace(/_/g, " ")));
      });
      if (closure && closure.root_key) {
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
:host { display:block; color:#f5f1df; background:#11130f; font:14px/1.45 "JetBrains Mono", "Fira Code", ui-monospace, monospace; }
* { box-sizing:border-box; }
.trace-page { min-height:100vh; background:radial-gradient(circle at 12% 0%, #344021 0, transparent 36rem), linear-gradient(140deg,#10140f,#18150f 58%,#101820); }
.trace-header { padding:28px 32px 16px; border-bottom:1px solid #333b2b; }
.eyebrow { margin:0 0 8px; color:#a7d982; text-transform:uppercase; letter-spacing:.16em; font-size:12px; }
h1 { margin:0; font:700 32px/1.1 Georgia,serif; }
.subtitle { max-width:1050px; margin:10px 0 0; color:#b8b098; }
.cards { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:12px; padding:16px 32px; }
.card,.panel { background:color-mix(in srgb,#191d16,transparent 6%); border:1px solid #343b2e; border-radius:18px; box-shadow:0 18px 48px #0007; }
.card { padding:13px 15px; min-width:0; }
.card span { display:block; color:#aaa58e; font-size:12px; text-transform:uppercase; letter-spacing:.08em; }
.card b { display:block; margin-top:4px; color:#f3bd55; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.workspace { display:grid; grid-template-columns:minmax(330px,1.15fr) repeat(3,minmax(260px,.8fr)); gap:14px; padding:0 32px 28px; align-items:start; }
.panel { overflow:hidden; min-height:260px; }
.source-panel { grid-row:span 2; }
.detail-panel { grid-column:span 2; }
.panel-head { padding:13px 15px; border-bottom:1px solid #343b2e; }
.panel-head h2 { margin:0; color:#9ad97f; font-size:13px; text-transform:uppercase; letter-spacing:.14em; }
.panel-head p { margin:5px 0 0; color:#8f957d; font-size:12px; }
.source-lines,.rows,.detail { max-height:520px; overflow:auto; }
.source-line { display:grid; grid-template-columns:48px minmax(0,1fr) auto; gap:10px; padding:0 14px; white-space:pre; cursor:pointer; align-items:center; }
.source-line code { color:#ede5c9; font:inherit; }
.ln { color:#6f765f; text-align:right; user-select:none; }
.trace-row { display:grid; grid-template-columns:minmax(70px,.45fr) minmax(75px,.45fr) minmax(140px,1fr) auto; width:100%; gap:8px; padding:9px 12px; border:0; border-bottom:1px solid #2c3229; background:transparent; color:#e7dec3; text-align:left; font:inherit; cursor:pointer; align-items:center; }
.trace-row:hover,.source-line:hover { background:#262817; }
.row-label { color:#f0bd58; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.row-meta { color:#7fb5ff; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.row-text { color:#bdb59d; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
[class*="trace-c-"] { transition:background .16s ease-out, color .16s ease-out, outline-color .16s ease-out; }
.trace-hover { background:#2a2b17 !important; outline:1px solid #7d6a31; outline-offset:-1px; }
.trace-selected { background:#4d3416 !important; color:#fff2be !important; outline:1px solid #f0bd58; outline-offset:-1px; }
.audit-good { box-shadow:inset 3px 0 #7fbf78; }
.audit-suspicious { box-shadow:inset 3px 0 #d97b53; }
.audit-optimized-attribution-gap { box-shadow:inset 3px 0 #f0bd58; }
.audit-source-span-sibling-unlowered,.audit-source-only-expected { box-shadow:inset 3px 0 #687464; }
.detail { padding:14px 15px; }
.muted { color:#8f957d; margin:0; }
.closure-card { padding:12px; border:1px solid #30382b; border-radius:12px; background:#11160f; margin-bottom:12px; }
.closure-card.audit-good { border-color:#3f6541; }
.closure-card.audit-suspicious { border-color:#805135; background:#17130d; }
.closure-card.audit-optimized-attribution-gap { border-color:#775f23; background:#17150d; }
.closure-card.audit-source-span-sibling-unlowered,.closure-card.audit-source-only-expected { border-color:#3a4231; background:#11160f; }
.closure-card h3 { margin:0 0 8px; color:#f0bd58; font-size:14px; }
.closure-card h4 { margin:12px 0 6px; color:#9ad97f; font-size:12px; text-transform:uppercase; letter-spacing:.1em; }
.phase-counts { margin:0 0 8px; color:#b8b098; font-size:12px; }
.gap { margin:8px 0; padding:8px 10px; border:1px solid #775f23; border-radius:9px; background:#2a2110; color:#ffd37a; }
.audit-note { margin:8px 0; padding:8px 10px; border:1px solid #453f29; border-radius:9px; background:#181910; color:#cfc8aa; }
.traversal,.selection-note { color:#8f957d; font-size:12px; margin:8px 0; }
.badges,.audit-header { display:flex; flex-wrap:wrap; gap:5px; align-items:center; justify-content:flex-end; min-width:0; }
.audit-header { justify-content:flex-start; margin:0 0 9px; }
.badge { display:inline-flex; align-items:center; max-width:150px; padding:2px 7px; border:1px solid #3a4231; border-radius:999px; color:#c9c2a8; background:#14190f; font-size:10px; line-height:1.4; text-transform:uppercase; letter-spacing:.06em; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.badge.phase { color:#7fb5ff; border-color:#31506f; background:#111927; }
.badge.ok { color:#a9dd86; border-color:#3f6541; background:#121b11; }
.badge.warn,.badge.symptom { color:#ffd37a; border-color:#775f23; background:#2a2110; }
.badge.group { color:#d7c99d; border-color:#56513a; }
.root-key { display:block; max-width:100%; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; color:#777d6a; font-size:11px; }
.phase-rail { display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:5px; margin:8px 0 10px; }
.phase-chip { padding:5px 6px; border:1px solid #30382b; border-radius:8px; color:#6f765f; background:#0d110c; text-align:center; font-size:11px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.phase-chip.reached { color:#d9d0b8; background:#181c12; border-color:#3a4231; }
.phase-chip.highest { color:#11130f; background:#f0bd58; border-color:#f0bd58; }
.span-group-card,.audit-summary { padding:12px; border:1px solid #3a4231; border-radius:12px; background:#11160f; margin-bottom:12px; }
.span-group-card h3,.audit-summary h3 { margin:0 0 8px; color:#9ad97f; font-size:13px; text-transform:uppercase; letter-spacing:.1em; }
.span-group { padding:9px 0; border-top:1px solid #252b22; }
.span-group:first-of-type { border-top:0; }
.span-title { margin:0 0 4px; color:#f0bd58; overflow-wrap:anywhere; }
.member-list h4 { margin:10px 0 5px; color:#8fc6ff; font-size:11px; text-transform:uppercase; letter-spacing:.08em; }
.member-row { display:grid; grid-template-columns:72px 96px minmax(0,1fr); width:100%; gap:7px; padding:5px 7px; border:0; border-radius:7px; background:transparent; color:#bdb59d; text-align:left; font:inherit; cursor:pointer; }
.member-row:hover { background:#232719; }
.member-class,.member-phase { color:#f0bd58; white-space:nowrap; }
.member-label { overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.audit-count { display:flex; justify-content:space-between; gap:10px; padding:4px 0; color:#aaa58e; border-top:1px solid #252b22; }
.audit-count b { color:#f0bd58; }
.edge,.span-line { display:grid; grid-template-columns:145px 1fr; gap:8px; padding:4px 0; color:#aeb59b; }
.edge-label { color:#7fb5ff; }
.edge-text,.span-line { overflow-wrap:anywhere; }
.notes { margin:0; padding:0 32px 30px 52px; color:#aaa58e; }
.notes li { margin:6px 0; }
@media (max-width:1250px) { .workspace,.cards { grid-template-columns:1fr 1fr; } .source-panel,.detail-panel { grid-column:span 2; } }
@media (max-width:760px) { .workspace,.cards { grid-template-columns:1fr; padding-left:14px; padding-right:14px; } .source-panel,.detail-panel { grid-column:span 1; } .trace-header { padding-left:14px; padding-right:14px; } .notes { padding-left:32px; padding-right:14px; } }
`;
    }
  }

  if (!customElements.get("fe-origin-trace")) {
    customElements.define("fe-origin-trace", FeOriginTrace);
  }
})();
