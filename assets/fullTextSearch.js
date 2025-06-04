const frames = $$(".embedded");
for (const f of frames) {
  f.onload = function () {
    f.style.height = f.contentWindow.document.body.scrollHeight + 50 + "px";
  };
}

// `MiniSearch` is already in global
window.miniSearch = new MiniSearch({
  fields: ["title", "text"], // fields to index for full-text search
  storeFields: ["title", "category"], // fields to return with search results
});

fetch("/search.json")
  .then((value) => {
    value.json().then((documents) => {
      window.miniSearch.addAll(documents);
    });
  })
  .catch((err) => console.error(err));

let dialogOpen = false;
document.addEventListener(
  "keydown",
  (event) => {
    const keyName = event.key;

    if (keyName === "Control" || keyName === "Meta") {
      // do not alert when only Control key is pressed.
      return;
    }

    if ((event.metaKey || event.ctrlKey) && keyName === "k") {
      const dialog = $("#search-dialog");
      if (dialogOpen) {
        $("#whole").classList.remove("blur");
        dialog.close();
        dialogOpen = !dialogOpen;
      } else {
        dialog.showModal();
        dialogOpen = !dialogOpen;
        $("#whole").classList.add("blur");
      }
    }
  },
  false
);

const input = $("#search-bar");
input.addEventListener(
  "input",
  function (evt) {
    let results = window.miniSearch.search(evt.target.value, {
      fields: ["addr", "taxon", "title"],
    });
    const search_result = $("#search-result");
    for (const obj of results) {
      if (obj.id === "index") {
        search_result.appendChild(
          span({}, `[${obj.id}] `, a({ href: `/` }, obj.title))
        );
      } else {
        search_result.appendChild(
          span({}, `[${obj.id}] `, a({ href: `/${obj.id}` }, obj.title))
        );
      }

      search_result.appendChild(br({}));
    }
    if (results.length === 0) {
      search_result.innerHTML = "";
    }
  },
  false
);
