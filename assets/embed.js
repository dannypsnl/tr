if (window.location !== window.parent.location) {
  // The page is in an iframe
  let frames = document.getElementsByTagName("iframe");
  for (let f of frames) {
    let toc = f.contentDocument.getElementById("toc");
    toc.style.display = "none";
  }
} else {
  // The page is not in an iframe
}
