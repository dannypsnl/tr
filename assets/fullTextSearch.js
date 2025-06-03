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
      search_result.appendChild(span({}, a({ href: `/${obj.id}` }, obj.title)));
    }
  },
  false
);
