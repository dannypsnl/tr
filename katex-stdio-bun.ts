import katex from "katex";

const decoder = new TextDecoder();
let buffer = "";

for await (const chunk of process.stdin) {
  buffer += decoder.decode(chunk);
  let nl;
  while ((nl = buffer.indexOf("\n")) !== -1) {
    const line = buffer.slice(0, nl);
    buffer = buffer.slice(nl + 1);
    const { display, tex } = JSON.parse(line);
    const html = katex.renderToString(tex, {
      throwOnError: false,
      displayMode: display
    });
    process.stdout.write(html);
    process.stdout.write("\0");
  }
}
