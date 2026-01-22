import { basicSetup, EditorView } from "https://esm.sh/codemirror@6.0.2";
import { githubDark } from "https://esm.sh/@fsegurai/codemirror-theme-github-dark";
import { gleam } from "https://esm.sh/@exercism/codemirror-lang-gleam";
import stdlib from "../stdlib.js";

import { Error as ResultError } from "../build/dev/javascript/gloat/gleam.mjs";
import * as gloatWeb from "../build/dev/javascript/gloat/gloat_web.mjs";
import * as gloat from "../build/dev/javascript/gloat/gloat.mjs";

const tenv = gloat.tenv_from_json(JSON.stringify(stdlib))[0];

const MODULE_KEY = "repl";
const encoder = new TextEncoder();

const hoverEl = document.getElementById("hover");
const statusText = document.getElementById("status-text");
const detailsEl = document.getElementById("details");
const targetSelect = document.getElementById("target");
const resetButton = document.getElementById("reset");

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

const view = new EditorView({
    doc: sample,
    extensions: [
        basicSetup,
        EditorView.updateListener.of((update) => {
            if (update.docChanged) scheduleTypecheck();
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
    scheduleTypecheck();
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
    console.log("tenv", tenv);
    const result = gloatWeb.analyze(source, target, MODULE_KEY, tenv);
    console.log(result);

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

function handleHover(event, view) {
    if (!hoverIndex.length || hoverTimer) {
        return;
    }

    hoverTimer = requestAnimationFrame(() => {
        hoverTimer = null;
        const pos = view.posAtCoords({ x: event.clientX, y: event.clientY });
        if (pos == null) {
            hideHover();
            return;
        }

        const offset = byteOffsetAtPos(view.state.doc.toString(), pos);
        const matches = hoverIndex
            .filter((entry) => entry.start <= offset && offset <= entry.end)
            .sort((a, b) => a.end - a.start - (b.end - b.start));

        if (!matches.length) {
            hideHover();
            return;
        }

        const types = [];
        for (const match of matches) {
            for (const type of match.types) {
                const typeString = type;
                if (!types.includes(typeString)) {
                    types.push(typeString);
                }
            }
            if (types.length) {
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

        hoverEl.textContent = types.join("\n");
        hoverEl.style.left = `${coords.left + 12}px`;
        hoverEl.style.top = `${coords.bottom + 12}px`;
        hoverEl.classList.remove("hidden");
    });
}

function hideHover() {
    hoverEl.classList.add("hidden");
}

function byteOffsetAtPos(source, pos) {
    return encoder.encode(source.slice(0, pos)).length;
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
