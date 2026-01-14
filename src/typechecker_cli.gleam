import glance
import gleam/bit_array
import gleam/dynamic
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gloat
import gloat/env

pub fn main() {
  case start_arguments() {
    [path] -> infer_file(path)
    _ -> usage()
  }
}

fn usage() {
  io.println_error("Usage: typechecker_cli <file.gleam>")
}

fn infer_file(path: String) {
  case read_file_text(path) {
    Ok(src) -> infer_source(src)
    Error(message) -> io.println_error("Failed to read file: " <> message)
  }
}

fn infer_source(src: String) {
  case glance.module(src) {
    Error(error) -> io.println_error("Parse error: " <> string.inspect(error))
    Ok(parsed) -> {
      let glance.Module(
        _imports,
        _custom_types,
        _type_aliases,
        constants,
        functions,
      ) = parsed
      let names =
        list.append(
          list.map(constants, fn(defn) {
            let glance.Definition(_attrs, constant) = defn
            let glance.Constant(_span, name, _publicity, _annotation, _value) =
              constant
            name
          }),
          list.map(functions, fn(defn) {
            let glance.Definition(_attrs, function) = defn
            let glance.Function(
              _span,
              name,
              _publicity,
              _parameters,
              _return,
              _body,
            ) = function
            name
          }),
        )
      case names {
        [] -> io.println("No top-level definitions found.")
        _ -> {
          let env_ = gloat.add_module(gloat.builtin_env(), parsed)
          let inferred =
            result.all(
              list.map(names, fn(name) {
                case env.resolve(env_, name) {
                  Ok(scheme) -> Ok(#(name, gloat.scheme_to_string(scheme)))
                  Error(_) -> Error("Definition not found in env: " <> name)
                }
              }),
            )
          case inferred {
            Ok(items) ->
              list.each(items, fn(item) {
                let #(name, scheme) = item
                io.println(name <> ": " <> scheme)
              })
            Error(message) -> io.println_error(message)
          }
        }
      }
    }
  }
}

@external(erlang, "init", "get_plain_arguments")
fn get_start_arguments() -> List(Charlist)

fn start_arguments() -> List(String) {
  get_start_arguments()
  |> list.map(charlist_to_string)
}

pub type Charlist

@external(erlang, "unicode", "characters_to_binary")
fn charlist_to_string(a: Charlist) -> String

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, dynamic.Dynamic)

fn read_file_text(path: String) -> Result(String, String) {
  case read_file(path) {
    Ok(bits) ->
      case bit_array.to_string(bits) {
        Ok(text) -> Ok(text)
        Error(_) -> Error("file is not valid UTF-8")
      }
    Error(error) -> Error(string.inspect(error))
  }
}
