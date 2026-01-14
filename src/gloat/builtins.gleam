import glance as g
import gleam/dict
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string
import gloat/env
import gloat/gleam_types
import gloat/infer
import gloat/infer_state as is
import gloat/scheme
import gloat/type_error
import gloat/types

pub fn add_def(
  tenv: env.TEnv,
  name: String,
  _name_loc: g.Span,
  expr: g.Expression,
  annotation: option.Option(g.Type),
  loc: g.Span,
  params: List(option.Option(String)),
) -> Result(env.TEnv, type_error.TypeError) {
  is.run_empty({
    use self <- is.bind(infer.new_type_var(name, types.unknown_span))
    let bound_env =
      tenv
      |> env.with_type(name, scheme.Forall(set.new(), self))
      |> env.with_type_params(name, params)
    use type_ <- is.bind(infer.infer_expr(bound_env, expr))
    use _ignored_annotation <- is.bind(case annotation {
      option.Some(type_expr) ->
        case gleam_types.type_(tenv, type_expr) {
          Ok(annot_type) -> infer.unify(type_, annot_type, loc)
          Error(_) -> is.error("Unsupported annotation", loc)
        }
      option.None -> is.ok(Nil)
    })
    use self_applied <- is.bind(is.apply_with(types.type_apply, self))
    use _ignored <- is.bind(infer.unify(self_applied, type_, loc))
    use type_applied <- is.bind(is.apply_with(types.type_apply, type_))
    is.ok(
      env.empty()
      |> env.with_type(name, env.generalize(tenv, type_applied))
      |> env.with_type_params(name, params),
    )
  })
}

pub fn add_defs(
  tenv: env.TEnv,
  defns: List(
    #(
      String,
      g.Span,
      g.Expression,
      option.Option(g.Type),
      g.Span,
      List(option.Option(String)),
    ),
  ),
) -> Result(env.TEnv, type_error.TypeError) {
  let names =
    list.map(defns, fn(args) {
      let #(name, _, _, _, _, _) = args
      name
    })
  let locs =
    list.map(defns, fn(args) {
      let #(_, _, _, _, loc, _) = args
      loc
    })
  let annotations =
    list.map(defns, fn(args) {
      let #(_, _, _, annotation, _, _) = args
      annotation
    })
  let params_list =
    list.map(defns, fn(args) {
      let #(_, _, _, _, _, params) = args
      params
    })

  is.run_empty({
    use vbls <- is.bind(
      is.map_list(defns, fn(args) {
        let #(name, _name_loc, _expr, _annotation, _loc, _params) = args
        infer.new_type_var(name, types.unknown_span)
      }),
    )
    let bound_env =
      list.fold(
        list.zip(names, list.zip(vbls, params_list)),
        tenv,
        fn(acc, args) {
          let #(name, #(vbl, params)) = args
          acc
          |> env.with_type(name, scheme.Forall(set.new(), vbl))
          |> env.with_type_params(name, params)
        },
      )

    use types_ <- is.bind(
      is.map_list(defns, fn(args) {
        let #(_, _, expr, _, _, _) = args
        infer.infer_expr(bound_env, expr)
      }),
    )
    use _ignored_annotations <- is.bind(
      is.each_list(list.zip(types_, list.zip(annotations, locs)), fn(args) {
        let #(type_, #(annotation, loc)) = args
        case annotation {
          option.None -> is.ok(Nil)
          option.Some(type_expr) ->
            case gleam_types.type_(tenv, type_expr) {
              Ok(annot_type) -> infer.unify(type_, annot_type, loc)
              Error(_) -> is.error("Unsupported annotation", loc)
            }
        }
      }),
    )
    use vbls_applied <- is.bind(
      is.map_list(vbls, fn(v) { is.apply_with(types.type_apply, v) }),
    )
    use _ignored <- is.bind(
      is.each_list(list.zip(vbls_applied, list.zip(types_, locs)), fn(args) {
        let #(vbl, #(type_, loc)) = args
        infer.unify(vbl, type_, loc)
      }),
    )
    use types_applied <- is.bind(
      is.map_list(types_, fn(t) { is.apply_with(types.type_apply, t) }),
    )
    let new_env =
      list.fold(
        list.zip(names, list.zip(types_applied, params_list)),
        env.empty(),
        fn(acc, args) {
          let #(name, #(type_, params)) = args
          acc
          |> env.with_type(name, env.generalize(tenv, type_))
          |> env.with_type_params(name, params)
        },
      )
    is.ok(new_env)
  })
}

pub fn add_typealias(
  _tenv: env.TEnv,
  name: String,
  args: List(#(String, g.Span)),
  type_: types.Type,
) -> env.TEnv {
  let free =
    list.map(args, fn(args) {
      let #(name, _) = args
      name
    })
  let alias_type = types.type_con_to_var(set.from_list(free), type_)
  env.TEnv(
    dict.new(),
    dict.new(),
    dict.new(),
    dict.from_list([#(name, #(free, alias_type))]),
    dict.new(),
    dict.new(),
  )
}

pub fn add_deftype(
  tenv: env.TEnv,
  name: String,
  args: List(#(String, g.Span)),
  constrs: List(
    #(String, g.Span, List(#(option.Option(String), types.Type)), g.Span),
  ),
  _loc: g.Span,
) -> env.TEnv {
  let free =
    list.map(args, fn(args) {
      let #(name, _) = args
      name
    })
  let free_set = set.from_list(free)
  let arg_types =
    list.map(args, fn(args) {
      let #(arg_name, _arg_loc) = args
      types.Tvar(arg_name, types.unknown_span)
    })
  let res = case arg_types {
    [] -> types.Tcon(name, types.unknown_span)
    _ ->
      types.Tapp(
        types.Tcon(name, types.unknown_span),
        arg_types,
        types.unknown_span,
      )
  }

  let env.TEnv(_values, _tcons, _types, aliases, _modules, _params) = tenv

  let parsed_constrs =
    list.map(constrs, fn(args) {
      let #(cname, _cloc, cargs, _cloc2) = args
      let args1 =
        list.map(cargs, fn(field) {
          let #(label, t) = field
          #(label, types.type_con_to_var(free_set, t))
        })
      let args2 =
        list.map(args1, fn(field) {
          let #(label, t) = field
          #(label, types.type_resolve_aliases(aliases, t))
        })
      #(cname, #(free, args2, res))
    })

  let values =
    dict.from_list(
      list.map(parsed_constrs, fn(args) {
        let #(cname, #(free2, cargs, cres)) = args
        let carg_types =
          list.map(cargs, fn(field) {
            let #(_label, t) = field
            t
          })
        let scheme_ =
          scheme.Forall(
            set.from_list(free2),
            types.tfns(carg_types, cres, types.unknown_span),
          )
        #(cname, scheme_)
      }),
    )

  let tcons = dict.from_list(parsed_constrs)
  let type_names =
    set.from_list(
      list.map(constrs, fn(args) {
        let #(cname, _, _, _) = args
        cname
      }),
    )
  let types_map = dict.from_list([#(name, #(list.length(args), type_names))])
  let params =
    dict.from_list(
      list.map(parsed_constrs, fn(args) {
        let #(cname, #(_free2, cargs, _cres)) = args
        let labels =
          list.map(cargs, fn(field) {
            let #(label, _t) = field
            label
          })
        #(cname, labels)
      }),
    )

  env.TEnv(values, tcons, types_map, dict.new(), dict.new(), params)
}

pub fn add_module(
  tenv: env.TEnv,
  module: g.Module,
) -> Result(env.TEnv, type_error.TypeError) {
  let g.Module(_imports, custom_types, type_aliases, constants, functions) =
    module
  result.try(add_imports(tenv, module), fn(import_env) {
    let tenv_with_imports = env.merge(tenv, import_env)
    result.try(
      fold_defs(type_aliases, env.empty(), fn(acc, defn) {
        result.map(add_typealias_def(tenv_with_imports, defn), fn(env_) {
          env.merge(acc, env_)
        })
      }),
      fn(alias_env) {
        let tenv_with_aliases = env.merge(tenv_with_imports, alias_env)
        result.try(
          fold_defs(custom_types, env.empty(), fn(acc, defn) {
            result.map(add_custom_type_def(tenv_with_aliases, defn), fn(env_) {
              env.merge(acc, env_)
            })
          }),
          fn(types_env) {
            let type_env = env.merge(alias_env, types_env)
            let tenv_with_types = env.merge(tenv_with_imports, type_env)
            result.try(
              result.map(
                result.all(list.map(functions, function_to_def)),
                fn(fn_defs) {
                  list.append(list.map(constants, constant_to_def), fn_defs)
                },
              ),
              fn(defs) {
                result.map(
                  add_defs_grouped(tenv_with_types, defs),
                  fn(defs_env) { env.merge(tenv_with_types, defs_env) },
                )
              },
            )
          },
        )
      },
    )
  })
}

fn add_imports(
  tenv: env.TEnv,
  module: g.Module,
) -> Result(env.TEnv, type_error.TypeError) {
  let g.Module(imports, _custom_types, _type_aliases, _constants, _functions) =
    module
  fold_defs(imports, env.empty(), fn(acc, defn) {
    let g.Definition(_attrs, import_) = defn
    let g.Import(span, module_name, alias, _types, values) = import_
    let base = module_key(module_name)
    let acc_with_module = case alias {
      option.None -> env.with_module(acc, base, base)
      option.Some(g.Named(name)) -> env.with_module(acc, name, base)
      option.Some(g.Discarded(_)) -> acc
    }
    fold_defs(values, acc_with_module, fn(acc2, value) {
      let g.UnqualifiedImport(name, alias) = value
      let target = case alias {
        option.None -> name
        option.Some(alias_name) -> alias_name
      }
      let qualified = base <> "/" <> name
      case env.resolve(tenv, qualified) {
        Ok(scheme_) -> {
          let acc3 = env.with_type(acc2, target, scheme_)
          let acc4 = case env.resolve_params(tenv, qualified) {
            Ok(params) -> env.with_type_params(acc3, target, params)
            Error(_) -> acc3
          }
          Ok(acc4)
        }
        Error(_) ->
          Error(type_error.new("Unknown import value " <> qualified, span))
      }
    })
  })
}

fn fold_defs(
  defs: List(a),
  init: b,
  f: fn(b, a) -> Result(b, type_error.TypeError),
) -> Result(b, type_error.TypeError) {
  case defs {
    [] -> Ok(init)
    [one, ..rest] ->
      result.try(f(init, one), fn(next) { fold_defs(rest, next, f) })
  }
}

fn module_key(module_name: String) -> String {
  let parts = string.split(module_name, "/")
  case list.reverse(parts) {
    [last, ..] -> last
    [] -> module_name
  }
}

fn add_defs_grouped(
  tenv: env.TEnv,
  defs: List(
    #(
      String,
      g.Span,
      g.Expression,
      option.Option(g.Type),
      g.Span,
      List(option.Option(String)),
    ),
  ),
) -> Result(env.TEnv, type_error.TypeError) {
  case defs {
    [] -> Ok(env.empty())
    _ -> {
      let def_groups = group_mutual_defs(defs)
      fold_defs(def_groups, env.empty(), fn(acc, group) {
        let merged = env.merge(tenv, acc)
        result.map(add_defs(merged, group), fn(group_env) {
          env.merge(acc, group_env)
        })
      })
    }
  }
}

fn add_typealias_def(
  tenv: env.TEnv,
  defn: g.Definition(g.TypeAlias),
) -> Result(env.TEnv, type_error.TypeError) {
  let g.Definition(_attrs, type_alias) = defn
  let g.TypeAlias(span, name, _publicity, parameters, aliased) = type_alias
  let args = list.map(parameters, fn(param) { #(param, span) })
  case gleam_types.type_(tenv, aliased) {
    Ok(type_) -> Ok(add_typealias(tenv, name, args, type_))
    Error(_) -> Error(type_error.new("Unsupported alias type", span))
  }
}

fn add_custom_type_def(
  tenv: env.TEnv,
  defn: g.Definition(g.CustomType),
) -> Result(env.TEnv, type_error.TypeError) {
  let g.Definition(_attrs, custom_type) = defn
  let g.CustomType(span, name, _publicity, _opaque, parameters, variants) =
    custom_type
  let args = list.map(parameters, fn(param) { #(param, span) })
  result.try(
    result.all(
      list.map(variants, fn(variant) {
        let g.Variant(cname, fields, _attributes) = variant
        result.map(
          result.map_error(gleam_types.variant_fields(tenv, fields), fn(_) {
            type_error.new("Unsupported constructor fields", span)
          }),
          fn(field_types) { #(cname, span, field_types, span) },
        )
      }),
    ),
    fn(constrs) { Ok(add_deftype(tenv, name, args, constrs, span)) },
  )
}

fn constant_to_def(
  defn: g.Definition(g.Constant),
) -> #(
  String,
  g.Span,
  g.Expression,
  option.Option(g.Type),
  g.Span,
  List(option.Option(String)),
) {
  let g.Definition(_attrs, constant) = defn
  let g.Constant(span, name, _publicity, annotation, value) = constant
  #(name, span, value, annotation, span, [])
}

fn function_to_def(
  defn: g.Definition(g.Function),
) -> Result(
  #(
    String,
    g.Span,
    g.Expression,
    option.Option(g.Type),
    g.Span,
    List(option.Option(String)),
  ),
  type_error.TypeError,
) {
  let g.Definition(attrs, function) = defn
  let g.Function(span, name, _publicity, parameters, return, body) = function
  let params =
    list.map(parameters, fn(param) {
      let g.FunctionParameter(label, name, _type) = param
      case label {
        option.Some(label) -> option.Some(label)
        option.None ->
          option.Some(case name {
            g.Discarded(name) -> name
            g.Named(name) -> name
          })
      }
    })
  case is_external(attrs) && body == [] {
    True -> {
      result.map(
        external_function_type(span, parameters, return),
        fn(annotation) {
          #(
            name,
            span,
            g.Todo(span, option.None),
            option.Some(annotation),
            span,
            params,
          )
        },
      )
    }
    False -> {
      let fn_params =
        list.map(parameters, fn(param) {
          let g.FunctionParameter(_label, name, type_) = param
          g.FnParameter(name, type_)
        })
      let expr = g.Fn(span, fn_params, return, body)
      Ok(#(name, span, expr, option.None, span, params))
    }
  }
}

fn is_external(attrs: List(g.Attribute)) -> Bool {
  list.any(attrs, fn(attr) {
    let g.Attribute(name, _arguments) = attr
    name == "external"
  })
}

fn external_function_type(
  span: g.Span,
  parameters: List(g.FunctionParameter),
  return: option.Option(g.Type),
) -> Result(g.Type, type_error.TypeError) {
  let args =
    result.all(
      list.map(parameters, fn(param) {
        let g.FunctionParameter(_label, _name, type_) = param
        case type_ {
          option.Some(type_) -> Ok(type_)
          option.None ->
            Error(type_error.new("External function missing annotation", span))
        }
      }),
    )
  result.try(args, fn(arg_types) {
    let return_type = case return {
      option.Some(type_) -> Ok(type_)
      option.None ->
        Error(type_error.new("External function missing return type", span))
    }
    result.map(return_type, fn(ret) { g.FunctionType(span, arg_types, ret) })
  })
}

type DefInfo =
  #(
    String,
    g.Span,
    g.Expression,
    option.Option(g.Type),
    g.Span,
    List(option.Option(String)),
    Int,
  )

fn group_mutual_defs(
  defs: List(
    #(
      String,
      g.Span,
      g.Expression,
      option.Option(g.Type),
      g.Span,
      List(option.Option(String)),
    ),
  ),
) -> List(
  List(
    #(
      String,
      g.Span,
      g.Expression,
      option.Option(g.Type),
      g.Span,
      List(option.Option(String)),
    ),
  ),
) {
  let defs_in_order = list.reverse(defs)
  let infos = def_infos(defs_in_order)
  let groups = def_info_groups(infos)
  list.map(groups, fn(group) {
    list.map(group, fn(info) {
      let #(name, name_loc, expr, annotation, loc, params, _idx) = info
      #(name, name_loc, expr, annotation, loc, params)
    })
  })
}

fn def_infos(
  defs: List(
    #(
      String,
      g.Span,
      g.Expression,
      option.Option(g.Type),
      g.Span,
      List(option.Option(String)),
    ),
  ),
) -> List(DefInfo) {
  let #(_idx, infos_rev) =
    list.fold(defs, #(0, []), fn(acc, def) {
      let #(idx, infos) = acc
      let #(name, name_loc, expr, annotation, loc, params) = def
      #(idx + 1, [
        #(name, name_loc, expr, annotation, loc, params, idx),
        ..infos
      ])
    })
  list.reverse(infos_rev)
}

fn def_info_groups(defs: List(DefInfo)) -> List(List(DefInfo)) {
  case defs {
    [] -> []
    _ -> {
      let names = list.map(defs, def_name)
      let names_set = set.from_list(names)
      let deps =
        dict.from_list(
          list.map(defs, fn(def) {
            let free = expr_free(def_expr(def), set.new())
            let dep_names =
              list.filter(set.to_list(free), fn(name) {
                set.contains(names_set, name)
              })
            #(def_name(def), set.from_list(dep_names))
          }),
        )
      let order = dfs_finish_order(names, deps, names)
      let reverse_deps = reverse_graph(names, deps)
      let sccs = dfs_sccs(order, reverse_deps, names)
      let sccs_topo = list.reverse(sccs)
      list.map(sccs_topo, fn(group_names) {
        let group_set = set.from_list(group_names)
        list.filter(defs, fn(def) { set.contains(group_set, def_name(def)) })
      })
    }
  }
}

fn def_name(def: DefInfo) -> String {
  let #(name, _, _, _, _, _, _) = def
  name
}

fn def_expr(def: DefInfo) -> g.Expression {
  let #(_, _, expr, _, _, _, _) = def
  expr
}

fn dfs_finish_order(
  names: List(String),
  graph: dict.Dict(String, set.Set(String)),
  name_order: List(String),
) -> List(String) {
  let #(_visited, order) =
    list.fold(names, #(set.new(), []), fn(acc, name) {
      let #(visited, order) = acc
      dfs_order(name, graph, name_order, visited, order)
    })
  order
}

fn dfs_order(
  name: String,
  graph: dict.Dict(String, set.Set(String)),
  name_order: List(String),
  visited: set.Set(String),
  order: List(String),
) -> #(set.Set(String), List(String)) {
  case set.contains(visited, name) {
    True -> #(visited, order)
    False -> {
      let visited = set.insert(visited, name)
      let neighbors = neighbors_in_order(name, graph, name_order)
      let #(visited2, order2) =
        list.fold(neighbors, #(visited, order), fn(acc, next) {
          let #(v, o) = acc
          dfs_order(next, graph, name_order, v, o)
        })
      #(visited2, [name, ..order2])
    }
  }
}

fn dfs_sccs(
  order: List(String),
  graph: dict.Dict(String, set.Set(String)),
  name_order: List(String),
) -> List(List(String)) {
  let #(_visited, comps_rev) =
    list.fold(order, #(set.new(), []), fn(acc, name) {
      let #(visited, comps) = acc
      case set.contains(visited, name) {
        True -> #(visited, comps)
        False -> {
          let #(visited2, comp) =
            dfs_collect(name, graph, name_order, visited, [])
          #(visited2, [comp, ..comps])
        }
      }
    })
  list.reverse(comps_rev)
}

fn dfs_collect(
  name: String,
  graph: dict.Dict(String, set.Set(String)),
  name_order: List(String),
  visited: set.Set(String),
  comp: List(String),
) -> #(set.Set(String), List(String)) {
  case set.contains(visited, name) {
    True -> #(visited, comp)
    False -> {
      let visited = set.insert(visited, name)
      let neighbors = neighbors_in_order(name, graph, name_order)
      let #(visited2, comp2) =
        list.fold(neighbors, #(visited, comp), fn(acc, next) {
          let #(v, c) = acc
          dfs_collect(next, graph, name_order, v, c)
        })
      #(visited2, [name, ..comp2])
    }
  }
}

fn neighbors_in_order(
  name: String,
  graph: dict.Dict(String, set.Set(String)),
  name_order: List(String),
) -> List(String) {
  case dict.get(graph, name) {
    Ok(dep_set) ->
      list.filter(name_order, fn(dep) { set.contains(dep_set, dep) })
    Error(_) -> []
  }
}

fn reverse_graph(
  names: List(String),
  graph: dict.Dict(String, set.Set(String)),
) -> dict.Dict(String, set.Set(String)) {
  let base =
    list.fold(names, dict.new(), fn(acc, name) {
      dict.insert(acc, name, set.new())
    })
  list.fold(names, base, fn(acc, name) {
    case dict.get(graph, name) {
      Ok(deps) ->
        list.fold(set.to_list(deps), acc, fn(acc2, dep) {
          let existing = case dict.get(acc2, dep) {
            Ok(s) -> s
            Error(_) -> set.new()
          }
          dict.insert(acc2, dep, set.insert(existing, name))
        })
      Error(_) -> acc
    }
  })
}

fn expr_free(expr: g.Expression, bound: set.Set(String)) -> set.Set(String) {
  case expr {
    g.Int(_, _) -> set.new()
    g.Float(_, _) -> set.new()
    g.String(_, _) -> set.new()
    g.Variable(_, name) ->
      case name {
        "True" -> set.new()
        "False" -> set.new()
        _ ->
          case set.contains(bound, name) {
            True -> set.new()
            False -> set.insert(set.new(), name)
          }
      }
    g.NegateInt(_, value) -> expr_free(value, bound)
    g.NegateBool(_, value) -> expr_free(value, bound)
    g.Block(_, statements) -> expr_free_block(statements, bound)
    g.Panic(_, message) ->
      case message {
        option.None -> set.new()
        option.Some(expr) -> expr_free(expr, bound)
      }
    g.Todo(_, message) ->
      case message {
        option.None -> set.new()
        option.Some(expr) -> expr_free(expr, bound)
      }
    g.Tuple(_, items) ->
      list.fold(items, set.new(), fn(acc, item) {
        set.union(acc, expr_free(item, bound))
      })
    g.List(_, items, tail) -> {
      let items_set =
        list.fold(items, set.new(), fn(acc, item) {
          set.union(acc, expr_free(item, bound))
        })
      case tail {
        option.None -> items_set
        option.Some(expr) -> set.union(items_set, expr_free(expr, bound))
      }
    }
    g.BitString(_, segments) ->
      list.fold(segments, set.new(), fn(acc, segment) {
        let #(expr, options) = segment
        let segment_free = expr_free(expr, bound)
        let options_free =
          list.fold(options, set.new(), fn(acc2, opt) {
            case opt {
              g.SizeValueOption(expr) -> set.union(acc2, expr_free(expr, bound))
              _ -> acc2
            }
          })
        set.union(acc, set.union(segment_free, options_free))
      })
    g.Echo(_, value, message) -> {
      let acc = case value {
        option.None -> set.new()
        option.Some(expr) -> expr_free(expr, bound)
      }
      case message {
        option.None -> acc
        option.Some(expr) -> set.union(acc, expr_free(expr, bound))
      }
    }
    g.RecordUpdate(_, _module, _name, record, fields) -> {
      let record_free = expr_free(record, bound)
      list.fold(fields, record_free, fn(acc, field) {
        let g.RecordUpdateField(label, item) = field
        let expr = case item {
          option.Some(expr) -> expr
          option.None -> g.Variable(g.Span(0, 0), label)
        }
        set.union(acc, expr_free(expr, bound))
      })
    }
    g.FieldAccess(_, container, _label) -> expr_free(container, bound)
    g.Fn(_, params, _return, body) -> {
      let bound_args =
        list.fold(params, set.new(), fn(acc, param) {
          let g.FnParameter(name, _type_) = param
          set.union(acc, bound_for_name(name))
        })
      expr_free_block(body, set.union(bound, bound_args))
    }
    g.Call(_, function, arguments) -> {
      let function_free = expr_free(function, bound)
      list.fold(arguments, function_free, fn(acc, arg) {
        case arg {
          g.UnlabelledField(item) -> set.union(acc, expr_free(item, bound))
          g.LabelledField(_label, _loc, item) ->
            set.union(acc, expr_free(item, bound))
          g.ShorthandField(label, _loc) ->
            case set.contains(bound, label) {
              True -> acc
              False -> set.insert(acc, label)
            }
        }
      })
    }
    g.TupleIndex(_, tuple, _index) -> expr_free(tuple, bound)
    g.FnCapture(_, _label, function, args_before, args_after) -> {
      let fn_free = expr_free(function, bound)
      let before_free =
        list.fold(args_before, set.new(), fn(acc, arg) {
          set.union(acc, expr_free_field(arg, bound))
        })
      let after_free =
        list.fold(args_after, set.new(), fn(acc, arg) {
          set.union(acc, expr_free_field(arg, bound))
        })
      set.union(fn_free, set.union(before_free, after_free))
    }
    g.Case(_, subjects, clauses) -> {
      let subjects_free =
        list.fold(subjects, set.new(), fn(acc, subject) {
          set.union(acc, expr_free(subject, bound))
        })
      let clauses_free =
        list.fold(clauses, set.new(), fn(acc, clause) {
          let g.Clause(patterns, guard, body) = clause
          let clause_free =
            list.fold(patterns, set.new(), fn(acc2, patterns) {
              let bound_case =
                list.fold(patterns, bound, fn(acc3, pat) {
                  set.union(acc3, pat_bound(pat))
                })
              let guard_free = case guard {
                option.None -> set.new()
                option.Some(expr) -> expr_free(expr, bound_case)
              }
              let body_free = expr_free(body, bound_case)
              set.union(acc2, set.union(guard_free, body_free))
            })
          set.union(acc, clause_free)
        })
      set.union(subjects_free, clauses_free)
    }
    g.BinaryOperator(_, _op, left, right) -> {
      let left_free = expr_free(left, bound)
      set.union(left_free, expr_free(right, bound))
    }
  }
}

fn expr_free_block(
  statements: List(g.Statement),
  bound: set.Set(String),
) -> set.Set(String) {
  case statements {
    [] -> set.new()
    [g.Expression(expr)] -> expr_free(expr, bound)
    [g.Expression(expr), ..rest] ->
      set.union(expr_free(expr, bound), expr_free_block(rest, bound))
    [g.Assignment(_, _kind, pat, _annotation, value), ..rest] -> {
      let free_value = expr_free(value, bound)
      let bound_next = set.union(bound, pat_bound(pat))
      set.union(free_value, expr_free_block(rest, bound_next))
    }
    [g.Assert(_, expr, message), ..rest] -> {
      let free_expr = expr_free(expr, bound)
      let free_msg = case message {
        option.None -> set.new()
        option.Some(expr) -> expr_free(expr, bound)
      }
      set.union(set.union(free_expr, free_msg), expr_free_block(rest, bound))
    }
    [g.Use(_, patterns, function), ..rest] -> {
      let free_fn = expr_free(function, bound)
      let bound_next = set.union(bound, use_patterns_bound(patterns))
      set.union(free_fn, expr_free_block(rest, bound_next))
    }
  }
}

fn expr_free_field(
  field: g.Field(g.Expression),
  bound: set.Set(String),
) -> set.Set(String) {
  case field {
    g.UnlabelledField(item) -> expr_free(item, bound)
    g.LabelledField(_label, _loc, item) -> expr_free(item, bound)
    g.ShorthandField(label, _loc) ->
      case set.contains(bound, label) {
        True -> set.new()
        False -> set.insert(set.new(), label)
      }
  }
}

fn use_patterns_bound(patterns: List(g.UsePattern)) -> set.Set(String) {
  list.fold(patterns, set.new(), fn(acc, pattern) {
    let g.UsePattern(pat, _annotation) = pattern
    set.union(acc, pat_bound(pat))
  })
}

fn bound_for_name(name: g.AssignmentName) -> set.Set(String) {
  case name {
    g.Named(name) -> set.insert(set.new(), name)
    g.Discarded(_) -> set.new()
  }
}

fn pat_bound(pat: g.Pattern) -> set.Set(String) {
  case pat {
    g.PatternDiscard(_, _) -> set.new()
    g.PatternVariable(_, name) -> set.insert(set.new(), name)
    g.PatternTuple(_, items) ->
      list.fold(items, set.new(), fn(acc, item) {
        set.union(acc, pat_bound(item))
      })
    g.PatternList(_, items, tail) -> {
      let items_set =
        list.fold(items, set.new(), fn(acc, item) {
          set.union(acc, pat_bound(item))
        })
      case tail {
        option.None -> items_set
        option.Some(pat) -> set.union(items_set, pat_bound(pat))
      }
    }
    g.PatternAssignment(_, pat, name) -> set.insert(pat_bound(pat), name)
    g.PatternConcatenate(_, _prefix, prefix_name, rest_name) -> {
      let acc = case prefix_name {
        option.None -> set.new()
        option.Some(name) ->
          case name {
            g.Named(name) -> set.insert(set.new(), name)
            g.Discarded(_) -> set.new()
          }
      }
      case rest_name {
        g.Named(name) -> set.insert(acc, name)
        g.Discarded(_) -> acc
      }
    }
    g.PatternBitString(_, segments) ->
      list.fold(segments, set.new(), fn(acc, segment) {
        let #(pat, _opts) = segment
        set.union(acc, pat_bound(pat))
      })
    g.PatternVariant(_, _module, _name, args, _spread) ->
      list.fold(args, set.new(), fn(acc, arg) {
        case arg {
          g.UnlabelledField(pat) -> set.union(acc, pat_bound(pat))
          g.LabelledField(_label, _loc, pat) -> set.union(acc, pat_bound(pat))
          g.ShorthandField(label, _loc) -> set.insert(acc, label)
        }
      })
    g.PatternString(_, _) -> set.new()
    g.PatternInt(_, _) -> set.new()
    g.PatternFloat(_, _) -> set.new()
  }
}

pub const tbool: types.Type = types.Tcon("Bool", types.unknown_span)

pub fn tmap(k: types.Type, v: types.Type) -> types.Type {
  types.Tapp(types.Tcon("Map", types.unknown_span), [k, v], types.unknown_span)
}

pub fn toption(arg: types.Type) -> types.Type {
  types.Tapp(
    types.Tcon("Option", types.unknown_span),
    [arg],
    types.unknown_span,
  )
}

pub fn tlist(arg: types.Type) -> types.Type {
  types.Tapp(types.Tcon("List", types.unknown_span), [arg], types.unknown_span)
}

pub fn tset(arg: types.Type) -> types.Type {
  types.Tapp(types.Tcon("Set", types.unknown_span), [arg], types.unknown_span)
}

pub fn concrete(t: types.Type) -> scheme.Scheme {
  scheme.Forall(set.new(), t)
}

pub fn generic(vbls: List(String), t: types.Type) -> scheme.Scheme {
  scheme.Forall(set.from_list(vbls), t)
}

pub fn vbl(k: String) -> types.Type {
  types.Tvar(k, types.unknown_span)
}

pub fn t_pair(a: types.Type, b: types.Type) -> types.Type {
  types.Ttuple([a, b], types.unknown_span)
}

pub const tstring: types.Type = types.Tcon("String", types.unknown_span)

pub fn builtin_env() -> env.TEnv {
  let k = vbl("k")
  let v = vbl("v")
  let v2 = vbl("v2")
  let kv = fn(t) { generic(["k", "v"], t) }
  let kk = fn(t) { generic(["k"], t) }
  let a = vbl("a")
  let b = vbl("b")

  env.TEnv(
    dict.from_list([
      #(
        "+",
        concrete(types.tfns(
          [types.tint, types.tint],
          types.tint,
          types.unknown_span,
        )),
      ),
      #(
        "<>",
        concrete(types.tfns([tstring, tstring], tstring, types.unknown_span)),
      ),
      #(
        "-",
        concrete(types.tfns(
          [types.tint, types.tint],
          types.tint,
          types.unknown_span,
        )),
      ),
      #(
        "negate",
        concrete(types.tfns([types.tint], types.tint, types.unknown_span)),
      ),
      #(
        ">",
        concrete(types.tfns([types.tint, types.tint], tbool, types.unknown_span)),
      ),
      #(
        "<",
        concrete(types.tfns([types.tint, types.tint], tbool, types.unknown_span)),
      ),
      #("=", generic(["k"], types.tfns([k, k], tbool, types.unknown_span))),
      #("!=", generic(["k"], types.tfns([k, k], tbool, types.unknown_span))),
      #(
        ">=",
        concrete(types.tfns([types.tint, types.tint], tbool, types.unknown_span)),
      ),
      #(
        "<=",
        concrete(types.tfns([types.tint, types.tint], tbool, types.unknown_span)),
      ),
      #("not", concrete(types.tfns([tbool], tbool, types.unknown_span))),
      #("()", concrete(types.Tcon("()", types.unknown_span))),
      #(
        ",",
        generic(
          ["a", "b"],
          types.tfns([a, b], t_pair(a, b), types.unknown_span),
        ),
      ),
      #(
        "trace",
        kk(types.tfns(
          [
            types.Tapp(
              types.Tcon("List", types.unknown_span),
              [
                types.Tapp(
                  types.Tcon("trace-fmt", types.unknown_span),
                  [k],
                  types.unknown_span,
                ),
              ],
              types.unknown_span,
            ),
          ],
          types.Tcon("()", types.unknown_span),
          types.unknown_span,
        )),
      ),
      #(
        "unescapeString",
        concrete(types.tfns([tstring], tstring, types.unknown_span)),
      ),
      #(
        "int-to-string",
        concrete(types.tfns([types.tint], tstring, types.unknown_span)),
      ),
      #(
        "int/to_string",
        concrete(types.tfns([types.tint], tstring, types.unknown_span)),
      ),
      #(
        "float/to_string",
        concrete(types.tfns(
          [types.Tcon("Float", types.unknown_span)],
          tstring,
          types.unknown_span,
        )),
      ),
      #(
        "string/append",
        concrete(types.tfns([tstring, tstring], tstring, types.unknown_span)),
      ),
      #(
        "string/repeat",
        concrete(types.tfns([tstring, types.tint], tstring, types.unknown_span)),
      ),
      #(
        "string/replace",
        concrete(types.tfns(
          [tstring, tstring, tstring],
          tstring,
          types.unknown_span,
        )),
      ),
      #(
        "string-to-int",
        concrete(types.tfns([tstring], toption(types.tint), types.unknown_span)),
      ),
      #(
        "string-to-float",
        concrete(types.tfns(
          [tstring],
          toption(types.Tcon("Float", types.unknown_span)),
          types.unknown_span,
        )),
      ),
      #("map/nil", kv(tmap(k, v))),
      #(
        "map/set",
        kv(types.tfns([tmap(k, v), k, v], tmap(k, v), types.unknown_span)),
      ),
      #(
        "map/rm",
        kv(types.tfns([tmap(k, v), k], tmap(k, v), types.unknown_span)),
      ),
      #(
        "map/get",
        kv(types.tfns([tmap(k, v), k], toption(v), types.unknown_span)),
      ),
      #(
        "map/map",
        generic(
          ["k", "v", "v2"],
          types.tfns(
            [types.tfns([v], v2, types.unknown_span), tmap(k, v)],
            tmap(k, v2),
            types.unknown_span,
          ),
        ),
      ),
      #(
        "map/merge",
        kv(types.tfns([tmap(k, v), tmap(k, v)], tmap(k, v), types.unknown_span)),
      ),
      #(
        "map/values",
        kv(types.tfns([tmap(k, v)], tlist(v), types.unknown_span)),
      ),
      #("map/keys", kv(types.tfns([tmap(k, v)], tlist(k), types.unknown_span))),
      #("set/nil", kk(tset(k))),
      #("set/add", kk(types.tfns([tset(k), k], tset(k), types.unknown_span))),
      #("set/has", kk(types.tfns([tset(k), k], tbool, types.unknown_span))),
      #("set/rm", kk(types.tfns([tset(k), k], tset(k), types.unknown_span))),
      #(
        "set/diff",
        kk(types.tfns([tset(k), tset(k)], tset(k), types.unknown_span)),
      ),
      #(
        "set/merge",
        kk(types.tfns([tset(k), tset(k)], tset(k), types.unknown_span)),
      ),
      #(
        "set/overlap",
        kk(types.tfns([tset(k), tset(k)], tset(k), types.unknown_span)),
      ),
      #("set/to-list", kk(types.tfns([tset(k)], tlist(k), types.unknown_span))),
      #(
        "set/from-list",
        kk(types.tfns([tlist(k)], tset(k), types.unknown_span)),
      ),
      #(
        "map/from-list",
        kv(types.tfns([tlist(t_pair(k, v))], tmap(k, v), types.unknown_span)),
      ),
      #(
        "map/to-list",
        kv(types.tfns([tmap(k, v)], tlist(t_pair(k, v)), types.unknown_span)),
      ),
      #(
        "jsonify",
        generic(
          ["v"],
          types.tfns(
            [types.Tvar("v", types.unknown_span)],
            tstring,
            types.unknown_span,
          ),
        ),
      ),
      #(
        "valueToString",
        generic(["v"], types.tfns([vbl("v")], tstring, types.unknown_span)),
      ),
      #(
        "eval",
        generic(
          ["v"],
          types.tfns(
            [types.Tcon("String", types.unknown_span)],
            vbl("v"),
            types.unknown_span,
          ),
        ),
      ),
      #(
        "eval-with",
        generic(
          ["ctx", "v"],
          types.tfns(
            [
              types.Tcon("ctx", types.unknown_span),
              types.Tcon("String", types.unknown_span),
            ],
            vbl("v"),
            types.unknown_span,
          ),
        ),
      ),
      #(
        "errorToString",
        generic(
          ["v"],
          types.tfns(
            [types.tfns([vbl("v")], tstring, types.unknown_span), vbl("v")],
            tstring,
            types.unknown_span,
          ),
        ),
      ),
      #(
        "sanitize",
        concrete(types.tfns([tstring], tstring, types.unknown_span)),
      ),
      #(
        "replace-all",
        concrete(types.tfns(
          [tstring, tstring, tstring],
          tstring,
          types.unknown_span,
        )),
      ),
      #(
        "fatal",
        generic(["v"], types.tfns([tstring], vbl("v"), types.unknown_span)),
      ),
    ]),
    dict.from_list([
      #("()", #([], [], types.Tcon("()", types.unknown_span))),
      #(",", #(["a", "b"], [#(option.None, a), #(option.None, b)], t_pair(a, b))),
      #("True", #([], [], tbool)),
      #("False", #([], [], tbool)),
      #("Nil", #([], [], types.Tcon("Nil", types.unknown_span))),
      #("Error", #(
        ["a", "b"],
        [#(option.None, b)],
        types.Tapp(
          types.Tcon("Result", types.unknown_span),
          [a, b],
          types.unknown_span,
        ),
      )),
      #("Ok", #(
        ["a", "b"],
        [#(option.None, a)],
        types.Tapp(
          types.Tcon("Result", types.unknown_span),
          [a, b],
          types.unknown_span,
        ),
      )),
      #("Some", #(
        ["a"],
        [#(option.None, a)],
        types.Tapp(
          types.Tcon("Option", types.unknown_span),
          [a],
          types.unknown_span,
        ),
      )),
      #("None", #(
        ["a"],
        [],
        types.Tapp(
          types.Tcon("Option", types.unknown_span),
          [a],
          types.unknown_span,
        ),
      )),
    ]),
    dict.from_list([
      #("Nil", #(0, set.new())),
      #("Int", #(0, set.new())),
      #("Float", #(0, set.new())),
      #("String", #(0, set.new())),
      #("Bool", #(0, set.new())),
      #("BitString", #(0, set.new())),
      #("Result", #(2, set.from_list(["Error", "Ok"]))),
      #("Option", #(1, set.from_list(["Some", "None"]))),
      #("List", #(1, set.new())),
      #("Map", #(2, set.new())),
      #("Set", #(1, set.new())),
    ]),
    dict.new(),
    dict.new(),
    dict.new(),
  )
}
