import glance
import gleam/bit_array
import gleam/dict
import gleam/dynamic
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import gloat
import gloat/env

pub fn main() {
  case parse_args(start_arguments()) {
    Ok(#(path, lib_dirs)) -> infer_file(path, lib_dirs)
    Error(message) -> {
      io.println_error(message)
      usage()
    }
  }
}

fn usage() {
  io.println_error("Usage: gloat_cli <file.gleam> [<lib_dir> ...]")
}

fn infer_file(path: String, lib_dirs: List(String)) {
  case read_file_text(path) {
    Ok(src) -> infer_source(src, path, lib_dirs)
    Error(message) -> io.println_error("Failed to read file: " <> message)
  }
}

fn infer_source(src: String, path: String, lib_dirs: List(String)) {
  case glance.module(src) {
    Error(error) -> io.println_error("Parse error: " <> string.inspect(error))
    Ok(parsed) -> {
      let search_dirs = [dir_of_path(path), ..lib_dirs]
      let glance.Module(
        imports,
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
          let env_ = gloat.builtin_env()
          case load_imports(env_, imports, search_dirs, set.new()) {
            Error(message) -> io.println_error(message)
            Ok(#(env_loaded, _visited)) -> {
              let env_final = gloat.add_module(env_loaded, parsed)
              let inferred =
                result.all(
                  list.map(names, fn(name) {
                    case env.resolve(env_final, name) {
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
  }
}

fn parse_args(args: List(String)) -> Result(#(String, List(String)), String) {
  case args {
    [] -> Error("Missing file path.")
    [path, ..rest] ->
      result.map(parse_lib_dirs(rest, []), fn(dirs) { #(path, dirs) })
  }
}

fn parse_lib_dirs(
  args: List(String),
  acc: List(String),
) -> Result(List(String), String) {
  case args {
    [] -> Ok(list.reverse(acc))
    ["--lib", dir, ..rest] -> parse_lib_dirs(rest, [dir, ..acc])
    ["-I", dir, ..rest] -> parse_lib_dirs(rest, [dir, ..acc])
    ["--lib"] -> Error("Missing directory after --lib")
    ["-I"] -> Error("Missing directory after -I")
    [dir, ..rest] -> parse_lib_dirs(rest, [dir, ..acc])
  }
}

fn load_imports(
  tenv: env.TEnv,
  imports: List(glance.Definition(glance.Import)),
  search_dirs: List(String),
  visited: set.Set(String),
) -> Result(#(env.TEnv, set.Set(String)), String) {
  case imports {
    [] -> Ok(#(tenv, visited))
    [defn, ..rest] -> {
      let glance.Definition(_attrs, import_) = defn
      let glance.Import(_loc, module_name, _alias, _types, _values) = import_
      result.try(
        load_module(tenv, module_name, search_dirs, visited),
        fn(state) {
          let #(env_loaded, visited_loaded) = state
          load_imports(env_loaded, rest, search_dirs, visited_loaded)
        },
      )
    }
  }
}

fn load_module(
  tenv: env.TEnv,
  module_name: String,
  search_dirs: List(String),
  visited: set.Set(String),
) -> Result(#(env.TEnv, set.Set(String)), String) {
  case set.contains(visited, module_name) {
    True -> Ok(#(tenv, visited))
    False -> {
      let visited = set.insert(visited, module_name)
      result.try(read_module_source(module_name, search_dirs), fn(args) {
        let #(_path, src) = args
        result.try(
          result.map_error(glance.module(src), fn(error) {
            "Parse error in " <> module_name <> ": " <> string.inspect(error)
          }),
          fn(parsed) {
            let glance.Module(imports, _custom_types, _type_aliases, _, _) =
              parsed
            result.try(
              load_imports(tenv, imports, search_dirs, visited),
              fn(loaded) {
                let #(env_loaded, visited_loaded) = loaded
                let module_env = gloat.add_module(env_loaded, parsed)
                result.map(
                  module_exports_env(module_name, module_env, parsed),
                  fn(exports) {
                    #(env.merge(env_loaded, exports), visited_loaded)
                  },
                )
              },
            )
          },
        )
      })
    }
  }
}

fn module_exports_env(
  module_name: String,
  module_env: env.TEnv,
  parsed: glance.Module,
) -> Result(env.TEnv, String) {
  let module_key = module_key(module_name)
  let value_names = module_value_names(parsed)
  let constructor_names = module_constructor_names(parsed)
  let exported = list.append(value_names, constructor_names)
  result.try(qualified_values(module_env, module_key, exported), fn(values) {
    let type_names = module_type_names(parsed)
    let alias_names = module_alias_names(parsed)
    let env.TEnv(_values, tcons, types, aliases, _modules) = module_env
    let tcons_filtered = filter_dict(tcons, constructor_names)
    let types_filtered = filter_dict(types, type_names)
    let aliases_filtered = filter_dict(aliases, alias_names)
    Ok(env.TEnv(
      values,
      tcons_filtered,
      types_filtered,
      aliases_filtered,
      dict.new(),
    ))
  })
}

fn qualified_values(
  module_env: env.TEnv,
  module_key: String,
  names: List(String),
) -> Result(dict.Dict(String, gloat.Scheme), String) {
  result.map(
    result.all(
      list.map(names, fn(name) {
        case env.resolve(module_env, name) {
          Ok(scheme) -> Ok(#(module_key <> "/" <> name, scheme))
          Error(_) ->
            Error(
              "Definition not found in module: " <> module_key <> "/" <> name,
            )
        }
      }),
    ),
    dict.from_list,
  )
}

fn module_value_names(parsed: glance.Module) -> List(String) {
  let glance.Module(
    _imports,
    _custom_types,
    _type_aliases,
    constants,
    functions,
  ) = parsed
  list.append(
    list.map(constants, fn(defn) {
      let glance.Definition(_attrs, constant) = defn
      let glance.Constant(_span, name, _publicity, _annotation, _value) =
        constant
      name
    }),
    list.map(functions, fn(defn) {
      let glance.Definition(_attrs, function) = defn
      let glance.Function(_span, name, _publicity, _parameters, _return, _body) =
        function
      name
    }),
  )
}

fn module_constructor_names(parsed: glance.Module) -> List(String) {
  let glance.Module(
    _imports,
    custom_types,
    _type_aliases,
    _constants,
    _functions,
  ) = parsed
  list.flatten(
    list.map(custom_types, fn(defn) {
      let glance.Definition(_attrs, custom) = defn
      let glance.CustomType(_span, _name, _pub, _opaque, _params, variants) =
        custom
      list.map(variants, fn(variant) {
        let glance.Variant(name, _fields, _attributes) = variant
        name
      })
    }),
  )
}

fn module_type_names(parsed: glance.Module) -> List(String) {
  let glance.Module(
    _imports,
    custom_types,
    _type_aliases,
    _constants,
    _functions,
  ) = parsed
  list.map(custom_types, fn(defn) {
    let glance.Definition(_attrs, custom) = defn
    let glance.CustomType(_span, name, _pub, _opaque, _params, _variants) =
      custom
    name
  })
}

fn module_alias_names(parsed: glance.Module) -> List(String) {
  let glance.Module(
    _imports,
    _custom_types,
    type_aliases,
    _constants,
    _functions,
  ) = parsed
  list.map(type_aliases, fn(defn) {
    let glance.Definition(_attrs, alias) = defn
    let glance.TypeAlias(_span, name, _pub, _params, _aliased) = alias
    name
  })
}

fn filter_dict(
  dict_: dict.Dict(String, a),
  names: List(String),
) -> dict.Dict(String, a) {
  let name_set = set.from_list(names)
  dict.to_list(dict_)
  |> list.filter(fn(entry) {
    let #(name, _value) = entry
    set.contains(name_set, name)
  })
  |> dict.from_list
}

fn read_module_source(
  module_name: String,
  search_dirs: List(String),
) -> Result(#(String, String), String) {
  let rel = module_name <> ".gleam"
  try_read_module(module_name, search_dirs, rel)
}

fn try_read_module(
  module_name: String,
  search_dirs: List(String),
  rel: String,
) -> Result(#(String, String), String) {
  case search_dirs {
    [] -> Error("Module not found: " <> module_name)
    [dir, ..rest] -> {
      let path = join_path(dir, rel)
      case read_file_text(path) {
        Ok(src) -> Ok(#(path, src))
        Error(_) -> try_read_module(module_name, rest, rel)
      }
    }
  }
}

fn join_path(base: String, rel: String) -> String {
  case string.ends_with(base, "/") {
    True -> base <> rel
    False -> base <> "/" <> rel
  }
}

fn dir_of_path(path: String) -> String {
  let parts = string.split(path, "/")
  case list.reverse(parts) {
    [_filename, ..rest_rev] -> {
      let rest = list.reverse(rest_rev)
      case rest {
        [] -> "."
        _ -> string.join(rest, "/")
      }
    }
    _ -> "."
  }
}

fn module_key(module_name: String) -> String {
  let parts = string.split(module_name, "/")
  case list.reverse(parts) {
    [last, ..] -> last
    [] -> module_name
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
