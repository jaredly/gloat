import { Ok, Error, List$Empty, List$NonEmpty, BitArray } from "./gleam.mjs";
import fs from "node:fs";

export function get_start_arguments() {
    return arrayToList(process.argv.slice(2));
}

export function charlist_to_string(value) {
    return value;
}

export function read_file(path) {
    try {
        const data = fs.readFileSync(path);
        return new Ok(new BitArray(new Uint8Array(data)));
    } catch (error) {
        return new Error(error);
    }
}

export function write_file(path, data) {
    try {
        fs.writeFileSync(path, data.rawBuffer);
        return new Ok(undefined);
    } catch (error) {
        return new Error(error);
    }
}

function arrayToList(array) {
    let list = List$Empty();
    let i = array.length;
    while (i--) {
        list = List$NonEmpty(array[i], list);
    }
    return list;
}
