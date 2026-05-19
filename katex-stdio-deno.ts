import katex from "npm:katex";

const decoder = new TextDecoder();
const encoder = new TextEncoder();
let buffer = "";

for await (const chunk of Deno.stdin.readable) {
  buffer += decoder.decode(chunk);
  let nl;
  while ((nl = buffer.indexOf("\n")) !== -1) {
    const line = buffer.slice(0, nl);
    buffer = buffer.slice(nl + 1);
    const displayMode = line[0] === "d";
    const formula = line.slice(1);
    const html = katex.renderToString(formula, {
      throwOnError: false,
      displayMode
    });
    await Deno.stdout.write(encoder.encode(html));
    await Deno.stdout.write(new Uint8Array([0]));
  }
}
