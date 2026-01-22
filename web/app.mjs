import { StateEffect, StateField } from "https://esm.sh/@codemirror/state";
import { Decoration } from "https://esm.sh/@codemirror/view";
import { gleam } from "https://esm.sh/@exercism/codemirror-lang-gleam";
import { githubDark } from "https://esm.sh/@fsegurai/codemirror-theme-github-dark";
import { basicSetup, EditorView } from "https://esm.sh/codemirror";
import { Error as ResultError } from "../build/dev/javascript/gloat/gleam.mjs";
import * as gloat from "../build/dev/javascript/gloat/gloat.mjs";
import * as gloatWeb from "../build/dev/javascript/gloat/gloat_web.mjs";
import stdlib from "../stdlib.js";

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
            if (update.selectionSet) handleSelectionHover(update.view);
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

function handleSelectionHover(view) {
    if (!hoverIndex.length || hoverTimer) {
        return;
    }

    hoverTimer = requestAnimationFrame(() => {
        hoverTimer = null;
        const selection = view.state.selection.main;
        if (!selection.empty) {
            hideHover();
            return;
        }
        const pos = selection.head;

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
    });
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
