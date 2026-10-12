// <tr-lazy src="/addr/embed.html"><details><summary>…</summary></details></tr-lazy>
// Fills the details with the card's embed the first time it is opened. Light
// DOM on purpose: style.css, katex.min.css and the numbering counters must
// reach the inserted content, and a shadow root would block all three.
customElements.define("tr-lazy", class extends HTMLElement {
  connectedCallback() {
    const details = this.querySelector(":scope > details");
    if (!details) return;
    let loading = false;
    details.addEventListener("toggle", async () => {
      if (!details.open || loading || this.hasAttribute("loaded")) return;
      loading = true;
      try {
        const res = await fetch(this.getAttribute("src"));
        if (!res.ok) return;
        const tpl = document.createElement("template");
        tpl.innerHTML = await res.text();
        // the same card may already be transcluded on this page; a duplicate
        // id would steal its anchor
        for (const el of tpl.content.querySelectorAll("[id]")) {
          if (document.getElementById(el.id)) el.removeAttribute("id");
        }
        details.append(tpl.content);
        this.setAttribute("loaded", "");
      } catch {
        // e.g. opened from file://; the heading still links to the page
      } finally {
        loading = false;
      }
    });
  }
});
