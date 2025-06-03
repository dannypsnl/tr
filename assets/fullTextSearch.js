// `MiniSearch` is already in global
window.miniSearch = new MiniSearch({
  fields: ["title", "text"], // fields to index for full-text search
  storeFields: ["title", "category"], // fields to return with search results
});

fetch("/search.json")
  .then((value) => {
    value.json().then((documents) => {
      window.miniSearch.addAll(documents);

      let results = window.miniSearch.search("first", {
        fields: ["addr", "taxon", "title"],
      });
      console.log({ results });
    });
  })
  .catch((err) => console.error(err));

document.addEventListener(
  "keydown",
  (event) => {
    const keyName = event.key;

    if (keyName === "Control" || keyName === "Meta") {
      // do not alert when only Control key is pressed.
      return;
    }

    if (event.metaKey || event.ctrlKey) {
      // TODO:
      // 1. toggle search bar (if OPEN => CLOSE, else CLOSE => OPEN)
    }
  },
  false
);
