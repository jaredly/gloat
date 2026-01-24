const isWhitespace = (char) => char === " " || char === "\n" || char === "\r" || char === "\t";

const isPlainObject = (value) => {
    return value !== null && typeof value === "object" && !Array.isArray(value);
};

const isTuple = (value) => value && value.type === "tuple";

const tokenize = (input) => {
    const tokens = [];
    let i = 0;
    while (i < input.length) {
        const char = input[i];
        if (isWhitespace(char)) {
            i += 1;
            continue;
        }
        if (char === "%") {
            while (i < input.length && input[i] !== "\n") {
                i += 1;
            }
            continue;
        }
        if (char === "{" || char === "}" || char === "[" || char === "]" || char === "," || char === ".") {
            tokens.push({ type: char, pos: i });
            i += 1;
            continue;
        }
        if (char === "<" && input[i + 1] === "<") {
            i += 2;
            if (input[i] !== '"') {
                throw new Error(`Expected binary string at ${i}`);
            }
            i += 1;
            let value = "";
            while (i < input.length) {
                const ch = input[i];
                if (ch === "\\") {
                    if (i + 1 >= input.length) {
                        throw new Error(`Unterminated escape at ${i}`);
                    }
                    value += input[i + 1];
                    i += 2;
                    continue;
                }
                if (ch === '"') {
                    i += 1;
                    break;
                }
                value += ch;
                i += 1;
            }
            let enc = "";
            if (input[i] === "/") {
                i++;
                while (i < input.length && input[i] !== ">") {
                    enc += input[i];
                    i++;
                }
            }
            if (input[i] !== ">" || input[i + 1] !== ">") {
                console.log(input.slice(i - 10, i + 10));
                throw new Error(`Expected >> after binary at ${i}`);
            }
            i += 2;
            if (input[i] === "/") {
                i += 1;
                while (i < input.length && /[A-Za-z0-9_-]/.test(input[i])) {
                    i += 1;
                }
            }
            tokens.push({ type: "binary", value, pos: i, enc });
            continue;
        }
        if (char >= "A" && char <= "z") {
            let name = char;
            i++;
            while (input[i] >= "A" && input[i] <= "z") {
                name += input[i];
                i++;
            }
            tokens.push({ type: "name", name, pos: i });
            continue;
        }
        console.log(input.slice(i - 10, i + 10));
        throw new Error(`Unexpected character '${char}' at ${i}`);
    }
    return tokens;
};

const parseList = (tokens, state) => {
    state.index += 1;
    const items = [];
    if (tokens[state.index]?.type === "]") {
        state.index += 1;
        return items;
    }
    while (state.index < tokens.length) {
        items.push(parseValue(tokens, state));
        const next = tokens[state.index];
        if (!next) break;
        if (next.type === ",") {
            state.index += 1;
            continue;
        }
        if (next.type === "]") {
            state.index += 1;
            return items;
        }
        throw new Error(`Expected ',' or ']' at ${next.pos}`);
    }
    throw new Error("Unterminated list");
};

const parseTuple = (tokens, state) => {
    state.index += 1;
    const left = parseValue(tokens, state);
    const comma = tokens[state.index];
    if (!comma || comma.type !== ",") {
        console.log(comma);
        throw new Error(`Expected ',' in tuple at ${comma?.pos ?? "end"}`);
    }
    state.index += 1;
    const right = parseValue(tokens, state);
    const closing = tokens[state.index];
    if (!closing || closing.type !== "}") {
        console.log(closing);
        throw new Error(`Expected '}' in tuple at ${closing?.pos ?? "end"}`);
    }
    state.index += 1;
    return { type: "tuple", values: [left, right] };
};

const parseValue = (tokens, state) => {
    const token = tokens[state.index];
    if (!token) {
        throw new Error("Unexpected end of input");
    }
    if (token.type === "binary") {
        state.index += 1;
        return token.value;
    }
    if (token.type === "[") {
        return parseList(tokens, state);
    }
    if (token.type === "{") {
        return parseTuple(tokens, state);
    }
    if (token.type === "name") {
        if (token.name === "true") {
            state.index += 1;
            return true;
        }
        if (token.name === "false") {
            state.index += 1;
            return false;
        }
        throw new Error(`Unexpected name ${token.name}`);
    }
    throw new Error(`Unexpected token '${token.type}' at ${token.pos}`);
};

const convertValue = (value) => {
    if (isTuple(value)) {
        const [key, inner] = value.values;
        if (typeof key === "string") {
            return { [key]: convertValue(inner) };
        }
        return value.values.map(convertValue);
    }
    if (Array.isArray(value)) {
        const converted = value.map(convertValue);
        if (converted.length && converted.every((item) => isPlainObject(item) && Object.keys(item).length === 1)) {
            return Object.assign({}, ...converted);
        }
        return converted;
    }
    return value;
};

const parseEntries = (input) => {
    const tokens = tokenize(input);
    const state = { index: 0 };
    const entries = [];
    while (state.index < tokens.length) {
        const term = parseValue(tokens, state);
        const dot = tokens[state.index];
        if (!dot || dot.type !== ".") {
            throw new Error(`Expected '.' after term at ${dot?.pos ?? "end"}`);
        }
        state.index += 1;
        entries.push(term);
    }
    return entries;
};

const parseErlangConfig = (input) => {
    const entries = parseEntries(input);
    const config = {};
    for (const entry of entries) {
        if (!isTuple(entry)) {
            throw new Error("Expected top-level tuples");
        }
        const [key, value] = entry.values;
        if (typeof key !== "string") {
            throw new Error("Expected binary keys in top-level tuples");
        }
        config[key] = convertValue(value);
    }
    return config;
};

export { parseErlangConfig };
