// KaTeX worker: reads one JSON request `{display, tex}` per line from stdin,
// writes the rendered HTML followed by a NUL byte to stdout.
// `private/katex/katex.min.js` is the official npm:katex@0.18.1 dist build,
// matching `default-assets/katex.min.css`.
"use strict";
const path = require("node:path");
const readline = require("node:readline");
const katex = require(path.join(__dirname, "private", "katex", "katex.min.js"));

const rl = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
rl.on("line", (line) => {
  if (line === "") return;
  const { display, tex } = JSON.parse(line);
  const html = katex.renderToString(tex, {
    throwOnError: false,
    displayMode: display
  });
  process.stdout.write(html + "\0");
});
