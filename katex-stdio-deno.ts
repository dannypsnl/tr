import katex from "npm:katex";

const decoder = new TextDecoder();
const encoder = new TextEncoder();
const writer = Deno.stdout.writable.getWriter();
let buffer = "";

for await (const chunk of Deno.stdin.readable) {
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
    await writer.write(encoder.encode(html));
    await writer.write(new Uint8Array([0]));
  }
}
