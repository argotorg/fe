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
        // If SCIP is available, try to resolve the link to a SCIP symbol
        // for richer hover information
        if (scip) {
          this._enrichLink(a, part.text, scip);
        }
        code.appendChild(a);
      } else {
        code.appendChild(document.createTextNode(part.text));
      }
    }

    this.innerHTML = "";
    this.appendChild(code);
  }

  /** Add SCIP hover info to a type link element. */
  _enrichLink(anchor, typeName, scip) {
    // Search for the type in SCIP to get hover info
    try {
      var results = JSON.parse(scip.search(typeName));
      for (var i = 0; i < results.length; i++) {
        if (results[i].display_name === typeName) {
          var info = scip.symbolInfo(results[i].symbol);
          if (info) {
            var parsed = JSON.parse(info);
            if (parsed.documentation && parsed.documentation.length > 0) {
              anchor.title = parsed.documentation[0].replace(/```[\s\S]*?```/g, "").trim();
            }
          }
          break;
        }
      }
    } catch (_) {}
  }
}

customElements.define("fe-signature", FeSignature);
