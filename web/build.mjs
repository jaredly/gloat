import { mkdir, rm, copyFile } from "fs/promises";
import { join } from "path";

const root = new URL(".", import.meta.url).pathname;
const distDir = join(root, "dist");
const entry = join(root, "app.mjs");

await rm(distDir, { recursive: true, force: true });
await mkdir(distDir, { recursive: true });

const result = await Bun.build({
    entrypoints: [entry],
    outdir: distDir,
    target: "browser",
    format: "esm",
    splitting: true,
    minify: true,
    sourcemap: "external",
});

if (!result.success) {
    console.error("Build failed.");
    process.exit(1);
}

await copyFile(join(root, "index.html"), join(distDir, "index.html"));
await copyFile(join(root, "styles.css"), join(distDir, "styles.css"));

console.log("Build complete.");
