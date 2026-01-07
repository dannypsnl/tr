import katex from "npm:katex";

Deno.serve({ port: 8765 }, async (req) => {
  try {
    const tex = await req.text();
    const html = katex.renderToString(tex, {
      throwOnError: false,
      displayMode: req.headers.get("Display-Mode") === "true"
    });
    return new Response(html);
  } catch (e) {
    return new Response(e.message, { status: 400 });
  }
});
