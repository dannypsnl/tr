import katex from "katex";

const decoder = new TextDecoder();
let buffer = "";

function writeAll(data: string | Uint8Array): Promise<void> {
  return new Promise((resolve, reject) => {
    process.stdout.write(data, (err) => err ? reject(err) : resolve());
  });
}

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
    await writeAll(html);
    await writeAll("\0");
  }
}
