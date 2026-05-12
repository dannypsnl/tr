import { createServer } from "node:http";
import katex from "katex";

const server = createServer((req, res) => {
  let body = "";
  req.on("data", (chunk) => { body += chunk; });
  req.on("end", () => {
    try {
      const html = katex.renderToString(body, {
        throwOnError: false,
        displayMode: req.headers["display-mode"] === "true"
      });
      res.writeHead(200);
      res.end(html);
    } catch (e) {
      res.writeHead(400);
      res.end(e.message);
    }
  });
});

server.listen(8765);
