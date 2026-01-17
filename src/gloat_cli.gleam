import glance
import gleam/bit_array
import gleam/dict
import gleam/dynamic
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string
import glexer.{Position}
import gloat
import gloat/env
import gloat/glance as gloat_glance
import gloat/scheme
import gloat/type_error
import gloat/types

pub fn main() {
  case parse_args(start_arguments()) {
    Ok(#(path, lib_dirs, target)) -> {
      infer_file(path, lib_dirs, target)
    }
    Error(message) -> {
      io.println_error(message)
      usage()
    }
  }
}

fn usage() {
  io.println_error(
    "Usage: gloat_cli <file.gleam> [--target <erlang|javascript>] [<lib_dir> ...]",
  )
}

fn infer_file(path: String, lib_dirs: List(String), target: String) {
  case read_file_text(path) {
    Ok(src) -> infer_source(src, path, lib_dirs, target)
    Error(message) -> io.println_error("Failed to read file: " <> message)
  }
}

fn infer_source(
  src: String,
  path: String,
  lib_dirs: List(String),
  target: String,
) {
  case glance.module(src) {
    Error(error) -> io.println_error(format_parse_error(path, src, error))
    Ok(parsed) -> {
      let parsed = gloat_glance.filter_module_for_target(parsed, target)
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
          case load_imports(env_, imports, search_dirs, set.new(), target) {
            Error(message) -> io.println_error(message)
            Ok(#(env_loaded, _visited)) -> {
              case gloat.add_module_with_target(env_loaded, parsed, target) {
                Error(err) ->
                  io.println_error(format_type_error(path, src, err))
                Ok(env_final) -> {
                  let inferred =
                    result.all(
                      list.map(names, fn(name) {
                        case env.resolve(env_final, name) {
                          Ok(scheme) ->
                            Ok(#(name, gloat.scheme_to_string(scheme)))
                          Error(_) ->
                            Error("Definition not found in env: " <> name)
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
  }
}

fn parse_args(
  args: List(String),
) -> Result(#(String, List(String), String), String) {
  case args {
    [] -> Error("Missing file path.")
    [path, ..rest] ->
      result.map(parse_lib_dirs(rest, [], "erlang"), fn(parsed) {
        let #(dirs, target) = parsed
        #(path, dirs, target)
      })
  }
}

fn parse_lib_dirs(
  args: List(String),
  acc: List(String),
  target: String,
) -> Result(#(List(String), String), String) {
  case args {
    [] -> Ok(#(list.reverse(acc), target))
    ["--target", target_name, ..rest] -> parse_lib_dirs(rest, acc, target_name)
    ["-t", target_name, ..rest] -> parse_lib_dirs(rest, acc, target_name)
    ["--lib", dir, ..rest] -> parse_lib_dirs(rest, [dir, ..acc], target)
    ["-I", dir, ..rest] -> parse_lib_dirs(rest, [dir, ..acc], target)
    ["--lib"] -> Error("Missing directory after --lib")
    ["-I"] -> Error("Missing directory after -I")
    [dir, ..rest] -> parse_lib_dirs(rest, [dir, ..acc], target)
  }
}

fn load_imports(
  tenv: env.TEnv,
  imports: List(glance.Definition(glance.Import)),
  search_dirs: List(String),
  visited: set.Set(String),
  target: String,
) -> Result(#(env.TEnv, set.Set(String)), String) {
  case imports {
    [] -> Ok(#(tenv, visited))
    [defn, ..rest] -> {
      let glance.Definition(_attrs, import_) = defn
      let glance.Import(_loc, module_name, _alias, _types, _values) = import_
      result.try(
        load_module(tenv, module_name, search_dirs, visited, target),
        fn(state) {
          let #(env_loaded, visited_loaded) = state
          load_imports(env_loaded, rest, search_dirs, visited_loaded, target)
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
  target: String,
) -> Result(#(env.TEnv, set.Set(String)), String) {
  case set.contains(visited, module_name) {
    True -> Ok(#(tenv, visited))
    False -> {
      let visited = set.insert(visited, module_name)
      result.try(read_module_source(module_name, search_dirs), fn(args) {
        let #(path, src) = args
        result.try(
          result.map_error(glance.module(src), fn(error) {
            format_parse_error(path, src, error)
          }),
          fn(parsed) {
            let parsed = gloat_glance.filter_module_for_target(parsed, target)
            let glance.Module(imports, _custom_types, _type_aliases, _, _) =
              parsed
            result.try(
              load_imports(tenv, imports, search_dirs, visited, target),
              fn(loaded) {
                let #(env_loaded, visited_loaded) = loaded
                result.try(
                  result.map_error(
                    gloat.add_module_with_target(env_loaded, parsed, target),
                    fn(err) { format_type_error(path, src, err) },
                  ),
                  fn(module_env) {
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
  let type_names = module_type_names(parsed)
  let alias_names = module_alias_names(parsed)
  let type_name_map = type_name_map(module_key, type_names, alias_names)
  result.try(
    qualified_values(module_env, module_key, exported, type_name_map),
    fn(values) {
      let env.TEnv(
        _values,
        tcons,
        types,
        aliases,
        _modules,
        _params,
        _type_names,
        _refinements,
      ) = module_env
      let tcons_filtered =
        filter_dict(tcons, constructor_names)
        |> qualify_tcons(module_key, type_name_map)
      let types_filtered =
        filter_dict(types, type_names)
        |> qualify_types_map(module_key)
      let aliases_filtered =
        filter_dict(aliases, alias_names)
        |> qualify_aliases(module_key, type_name_map)
      let params = qualified_params(module_env, module_key, exported)
      Ok(env.TEnv(
        values,
        tcons_filtered,
        types_filtered,
        aliases_filtered,
        dict.new(),
        params,
        dict.new(),
        dict.new(),
      ))
    },
  )
}

fn qualified_values(
  module_env: env.TEnv,
  module_key: String,
  names: List(String),
  type_name_map: dict.Dict(String, String),
) -> Result(dict.Dict(String, gloat.Scheme), String) {
  result.map(
    result.all(
      list.map(names, fn(name) {
        case env.resolve(module_env, name) {
          Ok(scheme) ->
            Ok(#(
              module_key <> "/" <> name,
              qualify_scheme(scheme, type_name_map),
            ))
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

fn qualified_params(
  module_env: env.TEnv,
  module_key: String,
  names: List(String),
) -> dict.Dict(String, List(option.Option(String))) {
  list.fold(names, dict.new(), fn(acc, name) {
    case env.resolve_params(module_env, name) {
      Ok(params) -> dict.insert(acc, module_key <> "/" <> name, params)
      Error(_) -> acc
    }
  })
}

fn qualify_tcons(
  tcons: dict.Dict(
    String,
    #(List(String), List(#(option.Option(String), types.Type)), types.Type),
  ),
  module_key: String,
  type_name_map: dict.Dict(String, String),
) -> dict.Dict(
  String,
  #(List(String), List(#(option.Option(String), types.Type)), types.Type),
) {
  dict.to_list(tcons)
  |> list.map(fn(entry) {
    let #(name, #(free, fields, res)) = entry
    let qualified_fields =
      list.map(fields, fn(field) {
        let #(label, type_) = field
        #(label, qualify_type(type_, type_name_map))
      })
    let qualified_res = qualify_type(res, type_name_map)
    #(module_key <> "/" <> name, #(free, qualified_fields, qualified_res))
  })
  |> dict.from_list
}

fn qualify_aliases(
  aliases: dict.Dict(String, #(List(String), types.Type)),
  module_key: String,
  type_name_map: dict.Dict(String, String),
) -> dict.Dict(String, #(List(String), types.Type)) {
  dict.to_list(aliases)
  |> list.map(fn(entry) {
    let #(name, #(free, alias_type)) = entry
    #(module_key <> "/" <> name, #(
      free,
      qualify_type(alias_type, type_name_map),
    ))
  })
  |> dict.from_list
}

fn qualify_types_map(
  types_map: dict.Dict(String, #(Int, set.Set(String))),
  module_key: String,
) -> dict.Dict(String, #(Int, set.Set(String))) {
  dict.to_list(types_map)
  |> list.map(fn(entry) {
    let #(name, #(arity, constructors)) = entry
    let qualified =
      list.map(set.to_list(constructors), fn(constructor) {
        module_key <> "/" <> constructor
      })
      |> set.from_list
    #(module_key <> "/" <> name, #(arity, qualified))
  })
  |> dict.from_list
}

fn type_name_map(
  module_key: String,
  type_names: List(String),
  alias_names: List(String),
) -> dict.Dict(String, String) {
  let names = list.append(type_names, alias_names)
  dict.from_list(
    list.map(names, fn(name) { #(name, module_key <> "/" <> name) }),
  )
}

fn qualify_scheme(
  scheme_: scheme.Scheme,
  type_name_map: dict.Dict(String, String),
) -> scheme.Scheme {
  let scheme.Forall(vbls, type_) = scheme_
  scheme.Forall(vbls, qualify_type(type_, type_name_map))
}

fn qualify_type(
  type_: types.Type,
  type_name_map: dict.Dict(String, String),
) -> types.Type {
  case type_ {
    types.Tvar(_, _) -> type_
    types.Tcon(name, span) ->
      case dict.get(type_name_map, name) {
        Ok(qualified) -> types.Tcon(qualified, span)
        Error(_) -> type_
      }
    types.Tapp(target, args, span) ->
      types.Tapp(
        qualify_type(target, type_name_map),
        list.map(args, fn(arg) { qualify_type(arg, type_name_map) }),
        span,
      )
    types.Ttuple(args, span) ->
      types.Ttuple(
        list.map(args, fn(arg) { qualify_type(arg, type_name_map) }),
        span,
      )
    types.Tfn(args, res, span) ->
      types.Tfn(
        list.map(args, fn(arg) { qualify_type(arg, type_name_map) }),
        qualify_type(res, type_name_map),
        span,
      )
  }
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

fn format_parse_error(
  path: String,
  source: String,
  error: glance.Error,
) -> String {
  case error {
    glance.UnexpectedEndOfInput -> {
      let offset = string.byte_size(source)
      format_span_message(
        "Parse error: unexpected end of input",
        path,
        source,
        glance.Span(offset, offset),
      )
    }
    glance.UnexpectedToken(token, position) -> {
      let Position(offset) = position
      format_span_message(
        "Parse error: unexpected token " <> string.inspect(token),
        path,
        source,
        glance.Span(offset, offset + 1),
      )
    }
  }
}

fn format_type_error(
  path: String,
  source: String,
  err: type_error.TypeError,
) -> String {
  format_span_message(
    "Type error: " <> type_error.message(err),
    path,
    source,
    type_error.span(err),
  )
}

fn format_span_message(
  message: String,
  path: String,
  source: String,
  span: glance.Span,
) -> String {
  let glance.Span(start, end) = span
  let #(line, col, line_text) = line_info(source, start)
  let caret_len = int.max(1, end - start)
  let padding = string.repeat(" ", col - 1)
  let caret = string.repeat("^", caret_len)
  message
  <> "\n --> "
  <> path
  <> ":"
  <> int.to_string(line)
  <> ":"
  <> int.to_string(col)
  <> "\n  |\n"
  <> int.to_string(line)
  <> " | "
  <> line_text
  <> "\n  | "
  <> padding
  <> caret
}

fn line_info(source: String, offset: Int) -> #(Int, Int, String) {
  let max_offset = string.byte_size(source)
  let capped = int.min(int.max(offset, 0), max_offset)
  let prefix = byte_slice(source, 0, capped)
  let prefix_lines = string.split(prefix, "\n")
  let line_no = list.length(prefix_lines)
  let col = case list.reverse(prefix_lines) {
    [last, ..] -> string.length(last) + 1
    [] -> 1
  }
  let line_text = list_nth(string.split(source, "\n"), line_no - 1, "")
  #(line_no, col, line_text)
}

fn byte_slice(source: String, start: Int, length: Int) -> String {
  let bits = bit_array.from_string(source)
  case bit_array.slice(bits, start, length) {
    Ok(slice) ->
      case bit_array.to_string(slice) {
        Ok(text) -> text
        Error(_) -> ""
      }
    Error(_) -> ""
  }
}

fn list_nth(list: List(a), index: Int, default: a) -> a {
  case list, index {
    [], _ -> default
    [head, ..], 0 -> head
    [_head, ..tail], _ -> list_nth(tail, index - 1, default)
  }
}

@external(erlang, "init", "get_plain_arguments")
@external(javascript, "./gloat_cli_ffi.mjs", "get_start_arguments")
fn get_start_arguments() -> List(Charlist)

fn start_arguments() -> List(String) {
  get_start_arguments()
  |> list.map(charlist_to_string)
}

pub type Charlist

@external(erlang, "unicode", "characters_to_binary")
@external(javascript, "./gloat_cli_ffi.mjs", "charlist_to_string")
fn charlist_to_string(a: Charlist) -> String

@external(erlang, "file", "read_file")
@external(javascript, "./gloat_cli_ffi.mjs", "read_file")
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
