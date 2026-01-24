import { StateEffect, StateField } from "https://esm.sh/@codemirror/state";
import { Decoration } from "https://esm.sh/@codemirror/view";
import { gleam } from "https://esm.sh/@exercism/codemirror-lang-gleam";
import { githubDark } from "https://esm.sh/@fsegurai/codemirror-theme-github-dark";
import { basicSetup, EditorView } from "https://esm.sh/codemirror";
import { gunzipSync } from "https://esm.sh/fflate@0.8.2";
import { Error as ResultError } from "../build/dev/javascript/gloat/gleam.mjs";
import { Error as GleamError, toList } from "../build/dev/javascript/gleam_stdlib/gleam.mjs";
import * as gleamDict from "../build/dev/javascript/gleam_stdlib/gleam/dict.mjs";
import * as gloat from "../build/dev/javascript/gloat/gloat.mjs";
import * as gloatWeb from "../build/dev/javascript/gloat/gloat_web.mjs";
import * as pubgrub from "../build/dev/javascript/pubgrub/pubgrub.mjs";
import * as pubgrubReport from "../build/dev/javascript/pubgrub/pubgrub/report.mjs";
import * as pubgrubVersion from "../build/dev/javascript/pubgrub/pubgrub/version.mjs";
import * as pubgrubRanges from "../build/dev/javascript/pubgrub/version_ranges.mjs";
// import stdlib from "../stdlib.js";
// import { Erlang } from "./erlang.mjs";
// import { Buffer } from "https://esm.sh/buffer";

const baseTenv = gloat.builtin_env();
// const baseTenv = gloat.tenv_from_json(JSON.stringify(stdlib))[0];
let tenv = baseTenv;
let packageSources = [];
const packageVersionCache = new Map();
const packageRequirementsCache = new Map();

const MODULE_KEY = "repl";
const encoder = new TextEncoder();

const hoverEl = document.getElementById("hover");
const statusText = document.getElementById("status-text");
const detailsEl = document.getElementById("details");
const targetSelect = document.getElementById("target");
const resetButton = document.getElementById("reset");
const packagesInput = document.getElementById("packages");
const loadPackagesButton = document.getElementById("load-packages");
const packagesStatus = document.getElementById("packages-status");

const sample = `import gleam/int
import gleam/list

pub fn sum(xs: List(Int)) -> Int {
  xs
  |> list.fold(0, fn(acc, item) { acc + item })
}

pub fn demo(x: Int) {
  let y = x + 10
  sum([y, 2, 3])
}`;

let hoverIndex = [];
let lastGoodEnv = null;
let lastSource = sample;
let hoverTimer = null;
let typecheckTimer = null;
let lastHoverRange = null;

const setHoverEffect = StateEffect.define();
const hoverField = StateField.define({
    create() {
        return Decoration.none;
    },
    update(value, tr) {
        let next = value.map(tr.changes);
        for (const effect of tr.effects) {
            if (effect.is(setHoverEffect)) {
                if (!effect.value) {
                    return Decoration.none;
                }
                const { from, to } = effect.value;
                return Decoration.set([Decoration.mark({ class: "cm-hover-highlight" }).range(from, to)]);
            }
        }
        return next;
    },
    provide: (field) => EditorView.decorations.from(field),
});

const view = new EditorView({
    doc: sample,
    extensions: [
        basicSetup,
        hoverField,
        EditorView.updateListener.of((update) => {
            if (update.docChanged) scheduleTypecheck();
            // if (update.selectionSet) handleSelectionHover(update.view);
        }),
        EditorView.domEventHandlers({
            mousemove: (event, view) => handleHover(event, view),
            mouseleave: () => hideHover(),
        }),
        githubDark,
        gleam(),
    ],
    parent: document.getElementById("editor"),
});

resetButton.addEventListener("click", () => {
    view.dispatch({
        changes: { from: 0, to: view.state.doc.length, insert: sample },
    });
});

targetSelect.addEventListener("change", () => {
    if (packageSources.length) {
        rebuildPackageEnv();
    }
    scheduleTypecheck();
});

loadPackagesButton.addEventListener("click", () => {
    loadPackages();
});

scheduleTypecheck();

function scheduleTypecheck() {
    clearTimeout(typecheckTimer);
    typecheckTimer = setTimeout(() => runTypecheck(), 200);
}

function runTypecheck() {
    const source = view.state.doc.toString();
    lastSource = source;
    const target = targetSelect.value;
    const result = gloatWeb.analyze(source, target, MODULE_KEY, tenv);

    if (result instanceof ResultError) {
        lastGoodEnv = null;
        hoverIndex = [];
        const err = result[0];
        const name = err?.constructor?.name ?? "AnalysisError";
        if (name === "ParseError") {
            setStatus("Parse error");
            detailsEl.textContent = err[0] || "Parse error.";
        } else if (name === "TypeError") {
            setStatus("Type error");
            detailsEl.textContent = formatTypeError(err, source);
        } else {
            setStatus("Error");
            detailsEl.textContent = "Analysis failed.";
        }
        return;
    }

    const nenv = result[0];
    const hovers = gloatWeb.hover_entries(nenv, MODULE_KEY);

    lastGoodEnv = hovers;
    hoverIndex = buildHoverIndex(lastGoodEnv);
    setStatus("Typecheck OK");
    detailsEl.textContent = `Hover over expressions to see types. (${hoverIndex.length} spans)`;
}

function buildHoverIndex(entries) {
    return entries.toArray().map(([start, end, types]) => ({
        start,
        end,
        types: types.toArray(),
    }));
}

async function loadPackages() {
    const specs = parsePackageSpecs(packagesInput.value);
    if (!specs.length) {
        setPackagesStatus("No packages specified.");
        return;
    }
    try {
        setPackagesStatus("Resolving dependencies...");
        const resolved = await resolvePackageVersions(specs);
        setPackagesStatus("Downloading packages...");
        const entries = [];
        for (const spec of resolved) {
            const { sources } = await fetchPackageSources(spec.name, spec.version);
            entries.push(...sources);
        }
        packageSources = entries;
        setPackagesStatus(`Loaded ${entries.length} modules.`);
        await rebuildPackageEnv();
        scheduleTypecheck();
    } catch (error) {
        setPackagesStatus(`Failed: ${error.message || error}`);
    }
}

async function rebuildPackageEnv() {
    if (!packageSources.length) {
        return;
    }
    const target = targetSelect.value;
    const jsonPayload = JSON.stringify(packageSources);
    const result = gloatWeb.load_sources_json(baseTenv, jsonPayload, target);
    if (result instanceof ResultError) {
        setPackagesStatus(`Decode failed: ${result[0]}`);
        return;
    }
    tenv = result[0];
}

function parsePackageSpecs(input) {
    return input
        .split(/[\n,]+/)
        .map((line) => line.trim())
        .filter(Boolean)
        .map((item) => {
            const [name, version] = item.split("@");
            return { name: name.trim(), version: version?.trim() || null };
        });
}

async function resolvePackageVersions(specs) {
    const rootPackage = "__root__";
    const rootVersion = [0, 0, 0];
    const compare = pubgrubVersion.compare;
    const normalized = await normalizePackageSpecs(specs);
    const provider = await buildOfflineProvider(normalized, rootPackage, rootVersion, compare);
    const resolved = pubgrub.resolve(provider, rootPackage, rootVersion);
    if (resolved instanceof GleamError) {
        const err = resolved[0];
        if (err instanceof pubgrub.NoSolution) {
            const tree = pubgrubReport.collapse_no_versions(err[0]);
            const formatter = pubgrubReport.default_report_formatter((pkg) => pkg, pubgrubVersion.to_string);
            const message = pubgrubReport.report(tree, formatter);
            throw new Error(`No solution: ${message}`);
        }
        throw new Error(`Dependency resolution failed: ${err.constructor?.name ?? err}`);
    }
    const solution = resolved[0];
    const entries = gleamDict.to_list(solution).toArray();
    return entries
        .map(([name, version]) => ({
            name,
            version: pubgrubVersion.to_string(version),
        }))
        .filter((entry) => entry.name !== rootPackage);
}

async function normalizePackageSpecs(specs) {
    const normalized = [];
    for (const spec of specs) {
        if (!spec.version) {
            const latest = await fetchLatestVersion(spec.name);
            normalized.push({
                name: spec.name,
                version: latest,
                requirement: latest,
                pinned: true,
            });
            continue;
        }
        const trimmed = spec.version.trim();
        const isExact = /^\d+(\.\d+){0,2}$/.test(trimmed);
        normalized.push({
            name: spec.name,
            version: trimmed,
            requirement: trimmed,
            pinned: isExact,
        });
    }
    return normalized;
}

async function buildOfflineProvider(specs, rootPackage, rootVersion, compare) {
    const provider = pubgrub.offline_new();
    const rootDeps = specs.map((spec) => [spec.name, parseRequirement(spec.requirement, compare)]);
    let updated = pubgrub.offline_add_dependencies(provider, rootPackage, rootVersion, toList(rootDeps));

    const rootPins = new Map(specs.filter((spec) => spec.pinned && spec.version).map((spec) => [spec.name, spec.version]));
    const graph = await buildDependencyGraph(specs, rootPins, compare);
    for (const [name, versions] of graph.versionsByPackage.entries()) {
        for (const version of versions) {
            const deps = graph.depsByPackageVersion.get(`${name}@${version}`) || [];
            updated = pubgrub.offline_add_dependencies(updated, name, parseVersion(version).value, toList(deps));
        }
    }

    return pubgrub.offline_provider(updated, compare);
}

async function buildDependencyGraph(specs, rootPins, compare) {
    const queue = specs.map((spec) => spec.name);
    const seen = new Set();
    const constraints = new Map();
    const versionsByPackage = new Map();
    const depsByPackageVersion = new Map();

    for (const spec of specs) {
        constraints.set(spec.name, parseRequirement(spec.requirement, compare));
    }

    while (queue.length) {
        const name = queue.shift();
        if (!name || seen.has(name)) {
            continue;
        }
        seen.add(name);
        const available = rootPins?.has(name) ? [rootPins.get(name)] : await fetchPackageVersions(name);
        const constraint = constraints.get(name);
        const versions = constraint ? available.filter((version) => pubgrubRanges.contains(constraint, parseVersion(version).value)) : available;
        versionsByPackage.set(name, versions);
        const requirementEntries = await Promise.all(
            versions.map(async (version) => {
                const requirements = await fetchPackageRequirements(name, version);
                return [version, requirementsToDeps(requirements)];
            }),
        );
        for (const [version, deps] of requirementEntries) {
            depsByPackageVersion.set(`${name}@${version}`, deps);
            for (const [depName, depRange] of deps) {
                const prev = constraints.get(depName);
                const next = prev ? pubgrubRanges.intersection(prev, depRange) : depRange;
                constraints.set(depName, next);
                if (!seen.has(depName)) {
                    queue.push(depName);
                }
            }
        }
    }

    return { versionsByPackage, depsByPackageVersion };
}

function parseRequirement(requirement, compare) {
    if (!requirement) {
        return pubgrubRanges.full(compare);
    }
    const orParts = requirement
        .split(/\s+or\s+/i)
        .map((part) => part.trim())
        .filter(Boolean)
        .map((part) => parseRequirementAnd(part, compare));
    if (!orParts.length) {
        return pubgrubRanges.full(compare);
    }
    return orParts.reduce((acc, next) => (acc ? pubgrubRanges.union(acc, next) : next), null);
}

function parseRequirementAnd(requirement, compare) {
    const parts = requirement
        .split(/\s+and\s+/i)
        .map((part) => part.trim())
        .filter(Boolean);
    let range = pubgrubRanges.full(compare);
    for (const part of parts) {
        range = pubgrubRanges.intersection(range, parseComparator(part, compare));
    }
    return range;
}

function parseComparator(token, compare) {
    if (!token || token === "*" || token.toLowerCase() === "any") {
        return pubgrubRanges.full(compare);
    }
    if (token.startsWith("~>")) {
        const parsed = parseVersion(token.slice(2).trim());
        if (parsed.segments === 0) {
            return pubgrubRanges.full(compare);
        }
        const lower = parsed.value;
        const upper = compatibleUpperBound(parsed.value, parsed.segments);
        return pubgrubRanges.between(compare, lower, upper);
    }
    if (token.startsWith(">=")) {
        const parsed = parseVersion(token.slice(2).trim());
        return parsed.segments === 0 ? pubgrubRanges.full(compare) : pubgrubRanges.higher_than(compare, parsed.value);
    }
    if (token.startsWith(">")) {
        const parsed = parseVersion(token.slice(1).trim());
        return parsed.segments === 0 ? pubgrubRanges.full(compare) : pubgrubRanges.strictly_higher_than(compare, parsed.value);
    }
    if (token.startsWith("<=")) {
        const parsed = parseVersion(token.slice(2).trim());
        return parsed.segments === 0 ? pubgrubRanges.full(compare) : pubgrubRanges.lower_than(compare, parsed.value);
    }
    if (token.startsWith("<")) {
        const parsed = parseVersion(token.slice(1).trim());
        return parsed.segments === 0 ? pubgrubRanges.full(compare) : pubgrubRanges.strictly_lower_than(compare, parsed.value);
    }
    if (token.startsWith("==")) {
        const parsed = parseVersion(token.slice(2).trim());
        return parsed.segments === 0 ? pubgrubRanges.full(compare) : pubgrubRanges.singleton(compare, parsed.value);
    }
    if (token.startsWith("=")) {
        const parsed = parseVersion(token.slice(1).trim());
        return parsed.segments === 0 ? pubgrubRanges.full(compare) : pubgrubRanges.singleton(compare, parsed.value);
    }
    const parsed = parseVersion(token);
    return parsed.segments === 0 ? pubgrubRanges.full(compare) : pubgrubRanges.singleton(compare, parsed.value);
}

function compatibleUpperBound(version, segments) {
    const [major, minor, patch] = version;
    if (segments === 1) {
        return [major + 1, 0, 0];
    }
    if (segments === 2) {
        if (major === 0) {
            return [major, minor + 1, 0];
        }
        return [major + 1, 0, 0];
    }
    return [major, minor + 1, 0];
}

function parseVersion(raw) {
    const match = raw.match(/^(\d+)(?:\.(\d+))?(?:\.(\d+))?/);
    if (!match) {
        return { value: [0, 0, 0], segments: 0 };
    }
    const segments = match.slice(1).filter((part) => part !== undefined).length;
    const major = Number(match[1] || 0);
    const minor = Number(match[2] || 0);
    const patch = Number(match[3] || 0);
    return { value: [major, minor, patch], segments };
}

const mainTar = async (name, resolved) => {
    const url = `https://cdn.jsdelivr.net/hex/tarballs/${name}-${resolved}.tar`;
    const response = await fetch(url);
    if (!response.ok) {
        throw new Error(`Failed to fetch ${name}@${resolved}`);
    }
    const buffer = new Uint8Array(await response.arrayBuffer());
    return extractTarGz(buffer);
};

const extractTarGz = (buffer) => {
    if (isGzip(buffer)) {
        buffer = gunzipSync(buffer);
    }
    return extractTar(buffer);
};

async function fetchPackageSources(name, version) {
    const resolved = version || (await fetchLatestVersion(name));
    const decoder = new TextDecoder();
    const mainEntries = await mainTar(name, resolved);
    const contents = mainEntries.find((e) => e.name === "contents.tar.gz");
    if (!contents) throw new Error(`no contents.tar.gz`);
    const contentsEntries = extractTarGz(contents.data);

    const sources = contentsEntries
        .map((entry) => {
            // console.log("contents entry", entry);
            const moduleName = moduleFromPath(entry.name);
            if (!moduleName) {
                return null;
            }
            return {
                module: moduleName,
                src: decoder.decode(entry.data),
            };
        })
        .filter(Boolean);
    return { sources };
}

async function fetchLatestVersion(name) {
    const response = await fetch(`https://hex.pm/api/packages/${name}`);
    if (!response.ok) {
        throw new Error(`Failed to resolve ${name} version`);
    }
    const data = await response.json();
    return (
        data.latest_version ||
        data.latest_stable_version ||
        (data.releases && data.releases[0] && data.releases[0].version) ||
        (() => {
            throw new Error(`No release info for ${name}`);
        })()
    );
}

async function fetchPackageVersions(name) {
    if (packageVersionCache.has(name)) {
        return packageVersionCache.get(name);
    }
    const response = await fetch(`https://hex.pm/api/packages/${name}`);
    if (!response.ok) {
        throw new Error(`Failed to resolve ${name} versions`);
    }
    const data = await response.json();
    const releases = data.releases || [];
    const versions = releases.map((release) => release.version).filter((version) => parseVersion(version).segments > 0);
    if (!versions.length && data.latest_version) {
        versions.push(data.latest_version);
    }
    packageVersionCache.set(name, versions);
    return versions;
}

async function fetchPackageRequirements(name, version) {
    const cacheKey = `${name}@${version}`;
    if (packageRequirementsCache.has(cacheKey)) {
        return packageRequirementsCache.get(cacheKey);
    }
    const response = await fetch(`https://hex.pm/api/packages/${name}/releases/${version}`);
    if (!response.ok) {
        throw new Error(`Failed to resolve ${name}@${version} requirements`);
    }
    const data = await response.json();
    const requirements = data.requirements || {};
    packageRequirementsCache.set(cacheKey, requirements);
    return requirements;
}

function requirementsToDeps(requirements) {
    if (!requirements || typeof requirements !== "object") {
        return [];
    }
    if (Array.isArray(requirements)) {
        return requirements
            .map((entry) => {
                if (!entry || entry.length < 2) {
                    return null;
                }
                const [name, info] = entry;
                return normalizeRequirementEntry(name, info);
            })
            .filter(Boolean);
    }
    return Object.entries(requirements)
        .map(([name, info]) => normalizeRequirementEntry(name, info))
        .filter(Boolean);
}

function normalizeRequirementEntry(name, info) {
    if (!name) {
        return null;
    }
    if (Array.isArray(info)) {
        const optional = info.some((value) => value === true);
        if (optional) {
            return null;
        }
        const requirement = info.find((value) => typeof value === "string" && /\d/.test(value)) || "";
        return [name, parseRequirement(requirement, pubgrubVersion.compare)];
    }
    if (info && typeof info === "object") {
        if (info.optional) {
            return null;
        }
        const requirement = info.requirement || info.version || info.req || "";
        return [name, parseRequirement(requirement, pubgrubVersion.compare)];
    }
    return [name, parseRequirement(String(info || ""), pubgrubVersion.compare)];
}

function isGzip(data) {
    return data.length > 2 && data[0] === 0x1f && data[1] === 0x8b;
}

function extractTar(data) {
    const entries = [];
    let offset = 0;
    while (offset + 512 <= data.length) {
        const header = data.subarray(offset, offset + 512);
        if (isAllZero(header)) {
            break;
        }
        const name = readString(header, 0, 100);
        const prefix = readString(header, 345, 155);
        const size = parseOctal(readString(header, 124, 12));
        const typeFlag = header[156];
        const fullName = prefix ? `${prefix}/${name}` : name;
        const dataStart = offset + 512;
        const dataEnd = dataStart + size;
        if (typeFlag !== 53 && size > 0) {
            entries.push({
                name: fullName,
                data: data.subarray(dataStart, dataEnd),
            });
        }
        offset = dataStart + Math.ceil(size / 512) * 512;
    }
    return entries;
}

function readString(buffer, start, length) {
    const slice = buffer.subarray(start, start + length);
    let end = 0;
    while (end < slice.length && slice[end] !== 0) {
        end += 1;
    }
    return new TextDecoder().decode(slice.subarray(0, end));
}

function parseOctal(value) {
    const trimmed = value.trim();
    if (!trimmed) {
        return 0;
    }
    return parseInt(trimmed, 8);
}

function isAllZero(buffer) {
    for (let i = 0; i < buffer.length; i += 1) {
        if (buffer[i] !== 0) {
            return false;
        }
    }
    return true;
}

function moduleFromPath(path) {
    const srcIndex = path.indexOf("/src/");
    let rel = null;
    if (srcIndex >= 0) {
        rel = path.slice(srcIndex + 5);
    } else if (path.startsWith("src/")) {
        rel = path.slice(4);
    }
    if (!rel || !rel.endsWith(".gleam")) {
        return null;
    }
    return rel.slice(0, -6);
}

function setPackagesStatus(message) {
    packagesStatus.textContent = message;
}

function handleHover(event, view) {
    if (!hoverIndex.length) {
        return;
    }
    if (hoverTimer) {
        clearTimeout(hoverTimer);
    }

    hoverTimer = setTimeout(() => {
        hoverTimer = null;
        const pos = view.posAtCoords({ x: event.clientX, y: event.clientY });
        // const selection = view.state.selection.main;
        if (!pos) {
            hideHover();
            return;
        }
        // const pos = selection.head;

        const offset = byteOffsetAtPos(view.state.doc.toString(), pos);
        const matches = hoverIndex
            .filter((entry) => entry.start <= offset && offset <= entry.end)
            .sort((a, b) => a.end - a.start - (b.end - b.start));

        if (!matches.length) {
            hideHover();
            return;
        }

        const types = [];
        let highlightSpan = null;
        for (const match of matches) {
            for (const type of match.types) {
                const typeString = type;
                if (!types.includes(typeString)) {
                    types.push(typeString);
                }
            }
            if (types.length && !highlightSpan) {
                highlightSpan = match;
            }
            if (types.length && highlightSpan) {
                break;
            }
        }

        if (!types.length) {
            hideHover();
            return;
        }

        const coords = view.coordsAtPos(pos);
        if (!coords) {
            hideHover();
            return;
        }

        highlightRange(view, highlightSpan);
        hoverEl.textContent = types.join("\n");
        hoverEl.style.left = `${coords.left + 12}px`;
        hoverEl.style.top = `${coords.bottom + 12}px`;
        hoverEl.classList.remove("hidden");
    }, 300);
}

function hideHover() {
    highlightRange(view, null);
    hoverEl.classList.add("hidden");
}

function byteOffsetAtPos(source, pos) {
    return encoder.encode(source.slice(0, pos)).length;
}

function posFromByteOffset(source, byteOffset) {
    if (byteOffset <= 0) {
        return 0;
    }
    let lo = 0;
    let hi = source.length;
    while (lo < hi) {
        const mid = Math.floor((lo + hi) / 2);
        const midBytes = encoder.encode(source.slice(0, mid)).length;
        if (midBytes < byteOffset) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    return lo;
}

function highlightRange(view, span) {
    if (!span) {
        if (lastHoverRange !== null) {
            lastHoverRange = null;
            view.dispatch({ effects: setHoverEffect.of(null) });
        }
        return;
    }
    const source = view.state.doc.toString();
    const from = posFromByteOffset(source, span.start);
    let to = posFromByteOffset(source, span.end);
    if (to <= from) {
        to = Math.min(from + 1, view.state.doc.length);
    }
    if (lastHoverRange && lastHoverRange.from === from && lastHoverRange.to === to) {
        return;
    }
    lastHoverRange = { from, to };
    view.dispatch({ effects: setHoverEffect.of({ from, to }) });
}

function setStatus(text) {
    statusText.textContent = text;
}

function formatTypeError(error, source) {
    if (!error) {
        return "Type error. (no error?)";
    }
    const message = error[0] || "Type error. (no message)";
    const start = error[1];
    const end = error[2];
    if (typeof start !== "number" || typeof end !== "number") {
        return message;
    }
    const preview = source.slice(start, end).trim();
    return preview ? `${message}\n\nAt: ${preview}` : message;
}
