const frames = document.getElementsByClassName("embedded");
for (const frame of frames) {
  frame.onload = function () {
    // set the height of the iframe as the height of the iframe content
    frame.style.height =
      frame.contentWindow.document.body.scrollHeight + 30 + "px";
  };
}
