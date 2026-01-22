import glance as g
import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string
import gloat
import gloat/env
import gloat/glance as gloat_glance
import gloat/scheme
import gloat/type_error
import gloat/types

pub type HoverEntry =
  #(Int, Int, List(String))

pub type AnalysisError {
  ParseError(message: String)
  TypeError(message: String, start: Int, end: Int)
}

pub fn analyze(
  src: String,
  target: String,
  module_key: String,
  tenv: gloat.TEnv,
) -> Result(gloat.TEnv, AnalysisError) {
  case g.module(src) {
    Error(err) -> Error(ParseError(format_parse_error(err)))
    Ok(parsed) -> {
      case gloat.add_module_with_target(tenv, parsed, target, module_key) {
        Error(err) -> Error(to_type_error(err))
        Ok(env_) -> Ok(env_)
      }
    }
  }
}

pub fn load_sources_json(
  tenv: env.TEnv,
  sources_json: String,
  target: String,
) -> Result(env.TEnv, String) {
  json.parse(sources_json, sources_decoder())
  |> result.map_error(fn(err) { string.inspect(err) })
  |> result.try(fn(entries) {
    let sources = dict.from_list(entries)
    load_sources(tenv, sources, target)
  })
}

pub fn hover_entries(env_: env.TEnv, module_key: String) -> List(HoverEntry) {
  case env.hover_for_module(env_, module_key) {
    Error(_) -> []
    Ok(hover_map) ->
      hover_map
      |> dict.to_list
      |> list.map(fn(entry) {
        let #(span, types_) = entry
        let g.Span(start, end) = span
        #(start, end, list.map(types_, types.type_to_string_gleam))
      })
  }
}

fn format_parse_error(error: g.Error) -> String {
  case error {
    g.UnexpectedEndOfInput -> "Unexpected end of input"
    g.UnexpectedToken(_token, _position) -> "Unexpected token"
  }
}

fn to_type_error(error: type_error.TypeError) -> AnalysisError {
  let g.Span(start, end) = type_error.span(error)
  TypeError(type_error.message(error), start, end)
}

fn sources_decoder() -> decode.Decoder(List(#(String, String))) {
  decode.list(of: source_entry_decoder())
}

fn source_entry_decoder() -> decode.Decoder(#(String, String)) {
  use module_name <- decode.field("module", decode.string)
  use src <- decode.field("src", decode.string)
  decode.success(#(module_name, src))
}

fn load_sources(
  tenv: env.TEnv,
  sources: dict.Dict(String, String),
  target: String,
) -> Result(env.TEnv, String) {
  let modules = dict.keys(sources)
  let visited = set.new()
  result.map(fold_modules(modules, tenv, visited, sources, target), fn(state) {
    let #(env_, _visited) = state
    env_
  })
}

fn fold_modules(
  modules: List(String),
  tenv: env.TEnv,
  visited: set.Set(String),
  sources: dict.Dict(String, String),
  target: String,
) -> Result(#(env.TEnv, set.Set(String)), String) {
  case modules {
    [] -> Ok(#(tenv, visited))
    [module_name, ..rest] -> {
      use #(env_loaded, visited_loaded) <- result.try(load_module(
        tenv,
        module_name,
        sources,
        visited,
        target,
      ))
      fold_modules(rest, env_loaded, visited_loaded, sources, target)
    }
  }
}

fn load_module(
  tenv: env.TEnv,
  module_name: String,
  sources: dict.Dict(String, String),
  visited: set.Set(String),
  target: String,
) -> Result(#(env.TEnv, set.Set(String)), String) {
  case set.contains(visited, module_name) {
    True -> Ok(#(tenv, visited))
    False -> {
      let visited = set.insert(visited, module_name)
      case dict.get(sources, module_name) {
        Error(_) -> Error("Module not found: " <> module_name)
        Ok(src) -> {
          use parsed <- result.try(
            result.map_error(g.module(src), fn(_err) {
              "Parse error in module: " <> module_name
            }),
          )
          let parsed = gloat_glance.filter_module_for_target(parsed, target)
          let g.Module(imports, _custom_types, _type_aliases, _, _) = parsed
          result.try(
            load_imports(tenv, imports, sources, visited, target),
            fn(loaded) {
              let #(env_loaded, visited_loaded) = loaded
              result.try(
                result.map_error(
                  gloat.add_module_with_target(
                    env_loaded,
                    parsed,
                    target,
                    module_name,
                  ),
                  fn(_err) { "Type error in module: " <> module_name },
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
        }
      }
    }
  }
}

fn load_imports(
  tenv: env.TEnv,
  imports: List(g.Definition(g.Import)),
  sources: dict.Dict(String, String),
  visited: set.Set(String),
  target: String,
) -> Result(#(env.TEnv, set.Set(String)), String) {
  case imports {
    [] -> Ok(#(tenv, visited))
    [defn, ..rest] -> {
      let g.Definition(_attrs, import_) = defn
      let g.Import(_loc, module_name, _alias, _types, _values) = import_
      result.try(
        load_module(tenv, module_name, sources, visited, target),
        fn(state) {
          let #(env_loaded, visited_loaded) = state
          load_imports(env_loaded, rest, sources, visited_loaded, target)
        },
      )
    }
  }
}

fn module_exports_env(
  module_name: String,
  module_env: env.TEnv,
  parsed: g.Module,
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
        _hover,
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

fn module_value_names(parsed: g.Module) -> List(String) {
  let g.Module(_imports, _custom_types, _type_aliases, constants, functions) =
    parsed
  list.append(
    list.map(constants, fn(defn) {
      let g.Definition(_attrs, constant) = defn
      let g.Constant(_span, name, _publicity, _annotation, _value) = constant
      name
    }),
    list.map(functions, fn(defn) {
      let g.Definition(_attrs, function) = defn
      let g.Function(_span, name, _publicity, _parameters, _return, _body) =
        function
      name
    }),
  )
}

fn module_constructor_names(parsed: g.Module) -> List(String) {
  let g.Module(_imports, custom_types, _type_aliases, _constants, _functions) =
    parsed
  list.flatten(
    list.map(custom_types, fn(defn) {
      let g.Definition(_attrs, custom) = defn
      let g.CustomType(_span, _name, _pub, _opaque, _params, variants) = custom
      list.map(variants, fn(variant) {
        let g.Variant(name, _fields, _attributes) = variant
        name
      })
    }),
  )
}

fn module_type_names(parsed: g.Module) -> List(String) {
  let g.Module(_imports, custom_types, _type_aliases, _constants, _functions) =
    parsed
  list.map(custom_types, fn(defn) {
    let g.Definition(_attrs, custom) = defn
    let g.CustomType(_span, name, _pub, _opaque, _params, _variants) = custom
    name
  })
}

fn module_alias_names(parsed: g.Module) -> List(String) {
  let g.Module(_imports, _custom_types, type_aliases, _constants, _functions) =
    parsed
  list.map(type_aliases, fn(defn) {
    let g.Definition(_attrs, alias) = defn
    let g.TypeAlias(_span, name, _pub, _params, _aliased) = alias
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

fn module_key(module_name: String) -> String {
  let parts = string.split(module_name, "/")
  case list.reverse(parts) {
    [last, ..] -> last
    [] -> module_name
  }
}
