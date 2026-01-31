import { watch } from "fs";
import { mkdir } from "fs/promises";
import { join } from "path";

const encoder = new TextEncoder();
const root = new URL(".", import.meta.url).pathname;
const distDir = join(root, "dist");
const entry = join(root, "app.mjs");
const port = Number(process.env.PORT || 3000);

await mkdir(distDir, { recursive: true });

const buildProcess = Bun.spawn({
    cmd: ["bun", "build", entry, "--outdir", distDir, "--target=browser", "--format=esm", "--splitting", "--sourcemap=inline", "--watch"],
    stdout: "inherit",
    stderr: "inherit",
});

const clients = new Set();

function notifyReload() {
    for (const controller of clients) {
        controller.enqueue(encoder.encode("data: reload\n\n"));
    }
}

const watcher = watch(distDir, { recursive: true }, () => {
    notifyReload();
});

async function resolveFile(pathname) {
    if (pathname === "/") return join(root, "index.html");
    const distPath = join(distDir, pathname);
    if (await Bun.file(distPath).exists()) return distPath;
    return join(root, pathname);
}

const liveScript = `\n<script type="module">\nconst source = new EventSource(\"/__live\");\nsource.onmessage = () => location.reload();\n</script>\n`;

const server = Bun.serve({
    port,
    async fetch(req) {
        const url = new URL(req.url);
        if (url.pathname === "/__live") {
            let controllerRef;
            const stream = new ReadableStream({
                start(controller) {
                    controllerRef = controller;
                    clients.add(controller);
                    controller.enqueue(encoder.encode("retry: 1000\n\n"));
                },
                cancel() {
                    clients.delete(controllerRef);
                },
            });
            return new Response(stream, {
                headers: {
                    "content-type": "text/event-stream",
                    "cache-control": "no-cache",
                    connection: "keep-alive",
                },
            });
        }

        if (url.pathname === "/" || url.pathname === "/index.html") {
            const html = await Bun.file(join(root, "index.html")).text();
            const injected = html.replace("</body>", `${liveScript}</body>`);
            return new Response(injected, { headers: { "content-type": "text/html" } });
        }

        const filePath = await resolveFile(url.pathname);
        const file = Bun.file(filePath);
        if (!(await file.exists())) return new Response("Not found", { status: 404 });
        return new Response(file);
    },
});

function shutdown() {
    watcher.close();
    buildProcess.kill();
    server.stop(true);
    process.exit(0);
}

process.on("SIGINT", shutdown);
process.on("SIGTERM", shutdown);

console.log(`Dev server running at http://localhost:${server.port}`);
