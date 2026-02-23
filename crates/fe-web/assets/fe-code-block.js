// <fe-code-block> — Custom element for syntax-highlighted Fe code blocks.
//
// Attributes:
//   highlighted  — content is pre-highlighted HTML (from Rust SSR)
//   lang         — language name (default "fe")
//   line-numbers — show line number gutter
//   collapsed    — start collapsed with <details>/<summary>

class FeCodeBlock extends HTMLElement {
  connectedCallback() {
    this.render();
  }

  render() {
    const highlighted = this.hasAttribute("highlighted");
    const lang = this.getAttribute("lang") || "fe";
    const showLineNumbers = this.hasAttribute("line-numbers");
    const collapsed = this.hasAttribute("collapsed");

    const wrapper = document.createElement("div");
    wrapper.className = "fe-code-block-wrapper";

    const pre = document.createElement("pre");
    pre.className = "fe-code-pre";

    const code = document.createElement("code");
    code.className = "language-" + lang;

    if (highlighted) {
      // Content is pre-rendered HTML from the Rust highlighter
      code.innerHTML = this.innerHTML;
    } else {
      // Runtime fallback: html-escape raw text content
      code.textContent = this.textContent;
    }

    // Clear original content before appending rendered version
    this.innerHTML = "";

    if (showLineNumbers) {
      var lines = code.innerHTML.split("\n");
      // Trim trailing empty line from trailing newline in source
      if (lines.length > 1 && lines[lines.length - 1] === "") {
        lines = lines.slice(0, -1);
      }
      const gutter = document.createElement("div");
      gutter.className = "fe-line-numbers";
      gutter.setAttribute("aria-hidden", "true");
      for (var i = 1; i <= lines.length; i++) {
        const span = document.createElement("span");
        span.textContent = i;
        gutter.appendChild(span);
      }
      wrapper.appendChild(gutter);
    }

    pre.appendChild(code);
    wrapper.appendChild(pre);

    if (collapsed) {
      const details = document.createElement("details");
      const summary = document.createElement("summary");
      summary.textContent = lang + " code";
      details.appendChild(summary);
      details.appendChild(wrapper);
      this.appendChild(details);
    } else {
      this.appendChild(wrapper);
    }
  }
}

customElements.define("fe-code-block", FeCodeBlock);
