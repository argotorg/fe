// <fe-signature> — Renders a type-linked function signature.
//
// Usage:
//   <fe-signature data='[{"text":"fn foo(","link":null},{"text":"Bar","link":"mylib::Bar/struct"}]'>
//   </fe-signature>
//
// Each entry in the JSON array has:
//   text — display text
//   link — if non-null, rendered as an <a> pointing to #link

class FeSignature extends HTMLElement {
  connectedCallback() {
    this.render();
  }

  render() {
    const raw = this.getAttribute("data");
    if (!raw) return;

    var parts;
    try {
      parts = JSON.parse(raw);
    } catch (_) {
      return;
    }

    const code = document.createElement("code");
    code.className = "fe-sig";

    var scip = window.FE_SCIP;

    for (var i = 0; i < parts.length; i++) {
      var part = parts[i];
      if (part.link) {
        var a = document.createElement("a");
        a.className = "type-link";
        a.href = "#" + part.link;
        a.textContent = part.text;
        // If SCIP is available, add symbol class for CSS-based highlighting
        if (scip) {
          this._enrichLink(a, part.link, part.text, scip);
        }
        code.appendChild(a);
      } else {
        code.appendChild(document.createTextNode(part.text));
      }
    }

    this.innerHTML = "";
    this.appendChild(code);
  }

  /** Add SCIP symbol class and hover tooltip to a type link element. */
  _enrichLink(anchor, docUrl, typeName, scip) {
    var symbol = scip.symbolForDocUrl(docUrl);

    // If we couldn't resolve via doc URL, fall back to name search
    if (!symbol) {
      try {
        var results = JSON.parse(scip.search(typeName));
        for (var i = 0; i < results.length; i++) {
          if (results[i].display_name === typeName) {
            symbol = results[i].symbol;
            break;
          }
        }
      } catch (_) {}
    }

    if (!symbol) return;

    var cls = scip.symbolClass(symbol);
    anchor.classList.add(cls);

    // Set hover tooltip from SCIP docs
    var info = scip.symbolInfo(symbol);
    if (info) {
      try {
        var parsed = JSON.parse(info);
        if (parsed.documentation && parsed.documentation.length > 0) {
          anchor.title = parsed.documentation[0].replace(/```[\s\S]*?```/g, "").trim();
        }
      } catch (_) {}
    }

    anchor.addEventListener("mouseenter", function () { feHighlight(cls); });
    anchor.addEventListener("mouseleave", feUnhighlight);
  }
}

customElements.define("fe-signature", FeSignature);
