import glance as g
import gleam/dict
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string
import gloat
import gloat/env
import gloat/scheme
import gloat/type_error
import gloat/types

pub type TypeOrParseError {
  Type(gloat.TypeError)
  Parse(g.Error)
}

pub fn assert_infer(code: String) -> Result(String, gloat.TypeError) {
  let module_code = "const top = {\n" <> code <> "\n}"
  result.map(
    infer_scheme_from_glance(module_code, "top"),
    gloat.scheme_to_string_gleam,
  )
}

pub fn assert_module_infer(
  code: String,
) -> Result(List(#(String, String)), TypeOrParseError) {
  infer_module(code, [])
}

pub fn assert_module_infer_with_deps(
  deps: List(#(String, String)),
  code: String,
) -> Result(List(#(String, String)), TypeOrParseError) {
  infer_module(code, deps)
}

pub fn sort_pairs(items: List(#(String, String))) -> List(#(String, String)) {
  list.sort(items, by: fn(a, b) {
    let #(name_a, _) = a
    let #(name_b, _) = b
    string.compare(name_a, name_b)
  })
}

fn infer_scheme_from_glance(
  code: String,
  name: String,
) -> Result(gloat.Scheme, gloat.TypeError) {
  let assert Ok(parsed) = g.module(code)
  result.try(
    gloat.add_module_with_target(gloat.builtin_env(), parsed, "erlang"),
    fn(env_) {
      case env.resolve(env_, name) {
        Ok(scheme_) -> Ok(scheme_)
        Error(_) ->
          Error(type_error.new(
            "definition not found in env",
            types.unknown_span,
          ))
      }
    },
  )
}

fn wrap_parse(res: Result(a, g.Error)) -> Result(a, TypeOrParseError) {
  case res {
    Ok(v) -> Ok(v)
    Error(err) -> Error(Parse(err))
  }
}

fn wrap_type(res: Result(a, gloat.TypeError)) -> Result(a, TypeOrParseError) {
  case res {
    Ok(v) -> Ok(v)
    Error(err) -> Error(Type(err))
  }
}

fn infer_module(
  code: String,
  deps: List(#(String, String)),
) -> Result(List(#(String, String)), TypeOrParseError) {
  let base_env = gloat.builtin_env()
  result.try(wrap_type(add_deps(base_env, deps)), fn(env_with_deps) {
    use parsed <- result.try(wrap_parse(g.module(code)))
    wrap_type(
      result.try(
        gloat.add_module_with_target(env_with_deps, parsed, "erlang"),
        fn(env2) {
          let names = public_export_names(parsed)
          result.map(
            result.all(
              list.map(names, fn(name) {
                case env.resolve(env2, name) {
                  Ok(scheme_) ->
                    Ok(#(name, gloat.scheme_to_string_gleam(scheme_)))
                  Error(_) ->
                    Error(type_error.new(
                      "definition not found in env: " <> name,
                      types.unknown_span,
                    ))
                }
              }),
            ),
            sort_pairs,
          )
        },
      ),
    )
  })
}

fn add_deps(
  base: env.TEnv,
  deps: List(#(String, String)),
) -> Result(env.TEnv, gloat.TypeError) {
  list.fold(deps, Ok(base), fn(acc, dep) {
    result.try(acc, fn(env_acc) {
      let #(module_name, src) = dep
      let assert Ok(parsed) = g.module(src)
      result.try(
        gloat.add_module_with_target(env_acc, parsed, "erlang"),
        fn(dep_env) {
          let qualified = qualify_dep_env(dep_env, parsed, module_name)
          Ok(env.merge(env_acc, qualified))
        },
      )
    })
  })
}

fn qualify_dep_env(
  dep_env: env.TEnv,
  module: g.Module,
  module_name: String,
) -> env.TEnv {
  let prefix = module_key(module_name) <> "/"
  let module_type_names = module_type_name_set(module)
  let public_values = public_value_names(module)
  let public_constructors = public_constructor_names(module)
  let public_aliases = public_type_alias_names(module)
  let public_custom_types = public_custom_type_names(module)

  let env.TEnv(
    _values,
    tcons,
    types_map,
    aliases,
    _modules,
    _params,
    _names,
    _refinements,
  ) = dep_env

  let values_dict =
    list.fold(public_values, dict.new(), fn(acc, name) {
      case env.resolve(dep_env, name) {
        Ok(scheme_) -> {
          let qualified = prefix <> name
          let updated = qualify_scheme(scheme_, module_type_names, prefix)
          dict.insert(acc, qualified, updated)
        }
        Error(_) -> acc
      }
    })

  let tcons_dict =
    list.fold(public_constructors, dict.new(), fn(acc, name) {
      case dict.get(tcons, name) {
        Ok(constructor) -> {
          let qualified = prefix <> name
          let updated = qualify_tcon(constructor, module_type_names, prefix)
          dict.insert(acc, qualified, updated)
        }
        Error(_) -> acc
      }
    })

  let params_dict =
    list.fold(
      list.append(public_values, public_constructors),
      dict.new(),
      fn(acc, name) {
        let qualified = prefix <> name
        case env.resolve_params(dep_env, name) {
          Ok(params) -> dict.insert(acc, qualified, params)
          Error(_) -> acc
        }
      },
    )

  let types_dict =
    list.fold(public_custom_types, dict.new(), fn(acc, name) {
      case dict.get(types_map, name) {
        Ok(#(arity, names)) -> {
          let qualified_names =
            names
            |> set.to_list
            |> list.map(fn(n) { prefix <> n })
            |> set.from_list
          dict.insert(acc, prefix <> name, #(arity, qualified_names))
        }
        Error(_) -> acc
      }
    })

  let alias_dict =
    list.fold(public_aliases, dict.new(), fn(acc, name) {
      case dict.get(aliases, name) {
        Ok(#(free, alias_type)) -> {
          let qualified = prefix <> name
          let updated = qualify_type(alias_type, module_type_names, prefix)
          dict.insert(acc, qualified, #(free, updated))
        }
        Error(_) -> acc
      }
    })

  env.TEnv(
    values_dict,
    tcons_dict,
    types_dict,
    alias_dict,
    dict.new(),
    params_dict,
    dict.new(),
    dict.new(),
  )
}

fn module_key(module_name: String) -> String {
  let parts = string.split(module_name, "/")
  case list.reverse(parts) {
    [last, ..] -> last
    [] -> module_name
  }
}

fn filter_map_option(items: List(a), f: fn(a) -> option.Option(b)) -> List(b) {
  list.fold(items, [], fn(acc, item) {
    case f(item) {
      option.Some(value) -> [value, ..acc]
      option.None -> acc
    }
  })
  |> list.reverse
}

fn public_export_names(module: g.Module) -> List(String) {
  let values = public_value_names(module)
  let constructors = public_constructor_names(module)
  list.append(values, constructors)
}

fn public_value_names(module: g.Module) -> List(String) {
  let g.Module(_imports, _types, _aliases, constants, functions) = module
  let fn_names =
    filter_map_option(functions, fn(defn) {
      let g.Definition(_attrs, function) = defn
      let g.Function(_span, name, publicity, _params, _return, _body) = function
      case publicity {
        g.Public -> option.Some(name)
        g.Private -> option.None
      }
    })
  let const_names =
    filter_map_option(constants, fn(defn) {
      let g.Definition(_attrs, constant) = defn
      let g.Constant(_span, name, publicity, _annotation, _value) = constant
      case publicity {
        g.Public -> option.Some(name)
        g.Private -> option.None
      }
    })
  list.append(fn_names, const_names)
}

fn public_constructor_names(module: g.Module) -> List(String) {
  let g.Module(_imports, custom_types, _aliases, _constants, _functions) =
    module
  list.fold(custom_types, [], fn(acc, defn) {
    let g.Definition(_attrs, custom_type) = defn
    let g.CustomType(_span, _name, publicity, opaque_, _params, variants) =
      custom_type
    case publicity, opaque_ {
      g.Public, False ->
        list.append(
          acc,
          list.map(variants, fn(variant) {
            let g.Variant(name, _fields, _attrs) = variant
            name
          }),
        )
      _, _ -> acc
    }
  })
}

fn public_type_alias_names(module: g.Module) -> List(String) {
  let g.Module(_imports, _types, type_aliases, _constants, _functions) = module
  filter_map_option(type_aliases, fn(defn) {
    let g.Definition(_attrs, alias) = defn
    let g.TypeAlias(_span, name, publicity, _params, _type) = alias
    case publicity {
      g.Public -> option.Some(name)
      g.Private -> option.None
    }
  })
}

fn public_custom_type_names(module: g.Module) -> List(String) {
  let g.Module(_imports, custom_types, _aliases, _constants, _functions) =
    module
  filter_map_option(custom_types, fn(defn) {
    let g.Definition(_attrs, custom_type) = defn
    let g.CustomType(_span, name, publicity, _opaque, _params, _variants) =
      custom_type
    case publicity {
      g.Public -> option.Some(name)
      g.Private -> option.None
    }
  })
}

fn module_type_name_set(module: g.Module) -> set.Set(String) {
  let g.Module(_imports, custom_types, type_aliases, _constants, _functions) =
    module
  let custom_names =
    list.map(custom_types, fn(defn) {
      let g.Definition(_attrs, custom_type) = defn
      let g.CustomType(_span, name, _publicity, _opaque, _params, _variants) =
        custom_type
      name
    })
  let alias_names =
    list.map(type_aliases, fn(defn) {
      let g.Definition(_attrs, alias) = defn
      let g.TypeAlias(_span, name, _publicity, _params, _type) = alias
      name
    })
  set.from_list(list.append(custom_names, alias_names))
}

fn qualify_scheme(
  scheme_: scheme.Scheme,
  module_type_names: set.Set(String),
  prefix: String,
) -> scheme.Scheme {
  let scheme.Forall(vbls, type_) = scheme_
  scheme.Forall(vbls, qualify_type(type_, module_type_names, prefix))
}

fn qualify_tcon(
  tcon: #(List(String), List(#(option.Option(String), types.Type)), types.Type),
  module_type_names: set.Set(String),
  prefix: String,
) -> #(List(String), List(#(option.Option(String), types.Type)), types.Type) {
  let #(free, fields, result) = tcon
  let updated_fields =
    list.map(fields, fn(field) {
      let #(label, field_type) = field
      #(label, qualify_type(field_type, module_type_names, prefix))
    })
  let updated_result = qualify_type(result, module_type_names, prefix)
  #(free, updated_fields, updated_result)
}

fn qualify_type(
  type_: types.Type,
  module_type_names: set.Set(String),
  prefix: String,
) -> types.Type {
  case type_ {
    types.Tvar(_, _) -> type_
    types.Tcon(name, span) ->
      case set.contains(module_type_names, name) {
        True -> types.Tcon(prefix <> name, span)
        False -> type_
      }
    types.Tapp(target, args, span) ->
      types.Tapp(
        qualify_type(target, module_type_names, prefix),
        list.map(args, fn(arg) { qualify_type(arg, module_type_names, prefix) }),
        span,
      )
    types.Ttuple(args, span) ->
      types.Ttuple(
        list.map(args, fn(arg) { qualify_type(arg, module_type_names, prefix) }),
        span,
      )
    types.Tfn(args, result, span) ->
      types.Tfn(
        list.map(args, fn(arg) { qualify_type(arg, module_type_names, prefix) }),
        qualify_type(result, module_type_names, prefix),
        span,
      )
  }
}
