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

    for (var i = 0; i < parts.length; i++) {
      var part = parts[i];
      if (part.link) {
        var a = document.createElement("a");
        a.className = "type-link";
        a.href = "#" + part.link;
        a.textContent = part.text;
        code.appendChild(a);
      } else {
        code.appendChild(document.createTextNode(part.text));
      }
    }

    this.innerHTML = "";
    this.appendChild(code);
  }
}

customElements.define("fe-signature", FeSignature);
