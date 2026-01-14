import glance as g
import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/set
import gleam/string
import gloat/env
import gloat/gleam_types
import gloat/infer
import gloat/runtime
import gloat/scheme
import gloat/state
import gloat/types

pub fn add_def(
  tenv: env.TEnv,
  name: String,
  name_loc: Int,
  expr: g.Expression,
  annotation: option.Option(g.Type),
  loc: Int,
) -> env.TEnv {
  state.run_empty({
    use self <- state.bind(infer.new_type_var(name, name_loc))
    let bound_env = env.with_type(tenv, name, scheme.Forall(set.new(), self))
    use type_ <- state.bind(infer.infer_expr(bound_env, expr))
    use _ignored_annotation <- state.bind(case annotation {
      option.Some(type_expr) ->
        case gleam_types.type_(type_expr) {
          Ok(annot_type) -> infer.unify(type_, annot_type, loc)
          Error(_) ->
            runtime.fatal("Unsupported annotation " <> int.to_string(loc))
        }
      option.None -> state.pure(Nil)
    })
    use self_applied <- state.bind(state.apply_with(types.type_apply, self))
    use _ignored <- state.bind(infer.unify(self_applied, type_, loc))
    use type_applied <- state.bind(state.apply_with(types.type_apply, type_))
    state.pure(env.with_type(
      env.empty(),
      name,
      env.generalize(tenv, type_applied),
    ))
  })
}

pub fn add_defs(
  tenv: env.TEnv,
  defns: List(#(String, Int, g.Expression, option.Option(g.Type), Int)),
) -> env.TEnv {
  let names =
    list.map(defns, fn(args) {
      let #(name, _, _, _, _) = args
      name
    })
  let locs =
    list.map(defns, fn(args) {
      let #(_, _, _, _, loc) = args
      loc
    })
  let annotations =
    list.map(defns, fn(args) {
      let #(_, _, _, annotation, _) = args
      annotation
    })

  state.run_empty({
    use vbls <- state.bind(
      state.map_list(defns, fn(args) {
        let #(name, name_loc, _expr, _annotation, _loc) = args
        infer.new_type_var(name, name_loc)
      }),
    )
    let bound_env =
      list.fold(
        list.zip(names, list.map(vbls, fn(v) { scheme.Forall(set.new(), v) })),
        tenv,
        fn(acc, args) {
          let #(name, vbl) = args
          env.with_type(acc, name, vbl)
        },
      )

    use types_ <- state.bind(
      state.map_list(defns, fn(args) {
        let #(_, _, expr, _, _) = args
        infer.infer_expr(bound_env, expr)
      }),
    )
    use _ignored_annotations <- state.bind(
      state.each_list(list.zip(types_, list.zip(annotations, locs)), fn(args) {
        let #(type_, #(annotation, loc)) = args
        case annotation {
          option.None -> state.pure(Nil)
          option.Some(type_expr) ->
            case gleam_types.type_(type_expr) {
              Ok(annot_type) -> infer.unify(type_, annot_type, loc)
              Error(_) ->
                runtime.fatal("Unsupported annotation " <> int.to_string(loc))
            }
        }
      }),
    )
    use vbls_applied <- state.bind(
      state.map_list(vbls, fn(v) { state.apply_with(types.type_apply, v) }),
    )
    use _ignored <- state.bind(
      state.each_list(list.zip(vbls_applied, list.zip(types_, locs)), fn(args) {
        let #(vbl, #(type_, loc)) = args
        infer.unify(vbl, type_, loc)
      }),
    )
    use types_applied <- state.bind(
      state.map_list(types_, fn(t) { state.apply_with(types.type_apply, t) }),
    )
    let new_env =
      list.fold(list.zip(names, types_applied), env.empty(), fn(acc, args) {
        let #(name, type_) = args
        env.with_type(acc, name, env.generalize(tenv, type_))
      })
    state.pure(new_env)
  })
}

pub fn add_typealias(
  _tenv: env.TEnv,
  name: String,
  args: List(#(String, Int)),
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
  )
}

pub fn add_deftype(
  tenv: env.TEnv,
  name: String,
  args: List(#(String, Int)),
  constrs: List(#(String, Int, List(#(option.Option(String), types.Type)), Int)),
  loc: Int,
) -> env.TEnv {
  let free =
    list.map(args, fn(args) {
      let #(name, _) = args
      name
    })
  let free_set = set.from_list(free)
  let arg_types =
    list.map(args, fn(args) {
      let #(arg_name, arg_loc) = args
      types.Tvar(arg_name, arg_loc)
    })
  let res = case arg_types {
    [] -> types.Tcon(name, loc)
    _ -> types.Tapp(types.Tcon(name, loc), arg_types, loc)
  }

  let env.TEnv(_values, _tcons, _types, aliases, _modules) = tenv

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
          scheme.Forall(set.from_list(free2), types.tfns(carg_types, cres, loc))
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

  env.TEnv(values, tcons, types_map, dict.new(), dict.new())
}

pub fn add_module(tenv: env.TEnv, module: g.Module) -> env.TEnv {
  let g.Module(_imports, custom_types, type_aliases, constants, functions) =
    module
  let import_env = add_imports(tenv, module)
  let tenv_with_imports = env.merge(tenv, import_env)
  let alias_env =
    list.fold(type_aliases, env.empty(), fn(acc, defn) {
      env.merge(acc, add_typealias_def(tenv_with_imports, defn))
    })
  let tenv_with_aliases = env.merge(tenv_with_imports, alias_env)
  let types_env =
    list.fold(custom_types, env.empty(), fn(acc, defn) {
      env.merge(acc, add_custom_type_def(tenv_with_aliases, defn))
    })
  let type_env = env.merge(alias_env, types_env)
  let tenv_with_types = env.merge(tenv_with_imports, type_env)
  let defs =
    list.append(
      list.map(constants, constant_to_def),
      list.map(functions, function_to_def),
    )
  let defs_env = add_defs_grouped(tenv_with_types, defs)
  env.merge(tenv_with_types, defs_env)
}

fn add_imports(tenv: env.TEnv, module: g.Module) -> env.TEnv {
  let g.Module(imports, _custom_types, _type_aliases, _constants, _functions) =
    module
  list.fold(imports, env.empty(), fn(acc, defn) {
    let g.Definition(_attrs, import_) = defn
    let g.Import(_loc, module_name, alias, _types, values) = import_
    let base = module_key(module_name)
    let acc_with_module = case alias {
      option.None -> env.with_module(acc, base, base)
      option.Some(g.Named(name)) -> env.with_module(acc, name, base)
      option.Some(g.Discarded(_)) -> acc
    }
    list.fold(values, acc_with_module, fn(acc2, value) {
      let g.UnqualifiedImport(name, alias) = value
      let target = case alias {
        option.None -> name
        option.Some(alias_name) -> alias_name
      }
      let qualified = base <> "/" <> name
      case env.resolve(tenv, qualified) {
        Ok(scheme_) -> env.with_type(acc2, target, scheme_)
        Error(_) -> runtime.fatal("Unknown import value " <> qualified)
      }
    })
  })
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
  defs: List(#(String, Int, g.Expression, option.Option(g.Type), Int)),
) -> env.TEnv {
  case defs {
    [] -> env.empty()
    _ -> {
      let def_groups = group_mutual_defs(defs)
      list.fold(def_groups, env.empty(), fn(acc, group) {
        let merged = env.merge(tenv, acc)
        env.merge(acc, add_defs(merged, group))
      })
    }
  }
}

fn add_typealias_def(
  tenv: env.TEnv,
  defn: g.Definition(g.TypeAlias),
) -> env.TEnv {
  let g.Definition(_attrs, type_alias) = defn
  let g.TypeAlias(span, name, _publicity, parameters, aliased) = type_alias
  let loc = gleam_types.loc_from_span(span)
  let args = list.map(parameters, fn(param) { #(param, loc) })
  let type_ = case gleam_types.type_(aliased) {
    Ok(type_) -> type_
    Error(_) -> runtime.fatal("Unsupported alias type " <> int.to_string(loc))
  }
  add_typealias(tenv, name, args, type_)
}

fn add_custom_type_def(
  tenv: env.TEnv,
  defn: g.Definition(g.CustomType),
) -> env.TEnv {
  let g.Definition(_attrs, custom_type) = defn
  let g.CustomType(span, name, _publicity, _opaque, parameters, variants) =
    custom_type
  let loc = gleam_types.loc_from_span(span)
  let args = list.map(parameters, fn(param) { #(param, loc) })
  let constrs =
    list.map(variants, fn(variant) {
      let g.Variant(cname, fields, _attributes) = variant
      let field_types = case gleam_types.variant_fields(fields) {
        Ok(field_types) -> field_types
        Error(_) ->
          runtime.fatal("Unsupported constructor fields " <> int.to_string(loc))
      }
      #(cname, loc, field_types, loc)
    })
  add_deftype(tenv, name, args, constrs, loc)
}

fn constant_to_def(
  defn: g.Definition(g.Constant),
) -> #(String, Int, g.Expression, option.Option(g.Type), Int) {
  let g.Definition(_attrs, constant) = defn
  let g.Constant(span, name, _publicity, annotation, value) = constant
  let loc = gleam_types.loc_from_span(span)
  #(name, loc, value, annotation, loc)
}

fn function_to_def(
  defn: g.Definition(g.Function),
) -> #(String, Int, g.Expression, option.Option(g.Type), Int) {
  let g.Definition(attrs, function) = defn
  let g.Function(span, name, _publicity, parameters, return, body) = function
  let loc = gleam_types.loc_from_span(span)
  case is_external(attrs) && body == [] {
    True -> {
      let annotation = external_function_type(span, parameters, return, loc)
      #(name, loc, g.Todo(span, option.None), option.Some(annotation), loc)
    }
    False -> {
      let fn_params =
        list.map(parameters, fn(param) {
          let g.FunctionParameter(_label, name, type_) = param
          g.FnParameter(name, type_)
        })
      let expr = g.Fn(span, fn_params, return, body)
      #(name, loc, expr, option.None, loc)
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
  loc: Int,
) -> g.Type {
  let args =
    list.map(parameters, fn(param) {
      let g.FunctionParameter(_label, _name, type_) = param
      case type_ {
        option.Some(type_) -> type_
        option.None ->
          runtime.fatal(
            "External function missing annotation " <> int.to_string(loc),
          )
      }
    })
  let return_type = case return {
    option.Some(type_) -> type_
    option.None ->
      runtime.fatal(
        "External function missing return type " <> int.to_string(loc),
      )
  }
  g.FunctionType(span, args, return_type)
}

type DefInfo =
  #(String, Int, g.Expression, option.Option(g.Type), Int, Int)

fn group_mutual_defs(
  defs: List(#(String, Int, g.Expression, option.Option(g.Type), Int)),
) -> List(List(#(String, Int, g.Expression, option.Option(g.Type), Int))) {
  let defs_in_order = list.reverse(defs)
  let infos = def_infos(defs_in_order)
  let groups = def_info_groups(infos)
  list.map(groups, fn(group) {
    list.map(group, fn(info) {
      let #(name, name_loc, expr, annotation, loc, _idx) = info
      #(name, name_loc, expr, annotation, loc)
    })
  })
}

fn def_infos(
  defs: List(#(String, Int, g.Expression, option.Option(g.Type), Int)),
) -> List(DefInfo) {
  let #(_idx, infos_rev) =
    list.fold(defs, #(0, []), fn(acc, def) {
      let #(idx, infos) = acc
      let #(name, name_loc, expr, annotation, loc) = def
      #(idx + 1, [#(name, name_loc, expr, annotation, loc, idx), ..infos])
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
  let #(name, _, _, _, _, _) = def
  name
}

fn def_expr(def: DefInfo) -> g.Expression {
  let #(_, _, expr, _, _, _) = def
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

pub const tbool: types.Type = types.Tcon("Bool", -1)

pub fn tmap(k: types.Type, v: types.Type) -> types.Type {
  types.Tapp(types.Tcon("Map", -1), [k, v], -1)
}

pub fn toption(arg: types.Type) -> types.Type {
  types.Tapp(types.Tcon("Option", -1), [arg], -1)
}

pub fn tlist(arg: types.Type) -> types.Type {
  types.Tapp(types.Tcon("List", -1), [arg], -1)
}

pub fn tset(arg: types.Type) -> types.Type {
  types.Tapp(types.Tcon("Set", -1), [arg], -1)
}

pub fn concrete(t: types.Type) -> scheme.Scheme {
  scheme.Forall(set.new(), t)
}

pub fn generic(vbls: List(String), t: types.Type) -> scheme.Scheme {
  scheme.Forall(set.from_list(vbls), t)
}

pub fn vbl(k: String) -> types.Type {
  types.Tvar(k, -1)
}

pub fn t_pair(a: types.Type, b: types.Type) -> types.Type {
  types.Ttuple([a, b], -1)
}

pub const tstring: types.Type = types.Tcon("String", -1)

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
      #("+", concrete(types.tfns([types.tint, types.tint], types.tint, -1))),
      #("<>", concrete(types.tfns([tstring, tstring], tstring, -1))),
      #("-", concrete(types.tfns([types.tint, types.tint], types.tint, -1))),
      #("negate", concrete(types.tfns([types.tint], types.tint, -1))),
      #(">", concrete(types.tfns([types.tint, types.tint], tbool, -1))),
      #("<", concrete(types.tfns([types.tint, types.tint], tbool, -1))),
      #("=", generic(["k"], types.tfns([k, k], tbool, -1))),
      #("!=", generic(["k"], types.tfns([k, k], tbool, -1))),
      #(">=", concrete(types.tfns([types.tint, types.tint], tbool, -1))),
      #("<=", concrete(types.tfns([types.tint, types.tint], tbool, -1))),
      #("not", concrete(types.tfns([tbool], tbool, -1))),
      #("()", concrete(types.Tcon("()", -1))),
      #(",", generic(["a", "b"], types.tfns([a, b], t_pair(a, b), -1))),
      #(
        "trace",
        kk(types.tfns(
          [
            types.Tapp(
              types.Tcon("List", -1),
              [types.Tapp(types.Tcon("trace-fmt", -1), [k], -1)],
              -1,
            ),
          ],
          types.Tcon("()", -1),
          -1,
        )),
      ),
      #("unescapeString", concrete(types.tfns([tstring], tstring, -1))),
      #("int-to-string", concrete(types.tfns([types.tint], tstring, -1))),
      #("int/to_string", concrete(types.tfns([types.tint], tstring, -1))),
      #(
        "float/to_string",
        concrete(types.tfns([types.Tcon("Float", -1)], tstring, -1)),
      ),
      #("string/append", concrete(types.tfns([tstring, tstring], tstring, -1))),
      #(
        "string/repeat",
        concrete(types.tfns([tstring, types.tint], tstring, -1)),
      ),
      #(
        "string/replace",
        concrete(types.tfns([tstring, tstring, tstring], tstring, -1)),
      ),
      #(
        "string-to-int",
        concrete(types.tfns([tstring], toption(types.tint), -1)),
      ),
      #(
        "string-to-float",
        concrete(types.tfns([tstring], toption(types.Tcon("Float", -1)), -1)),
      ),
      #("map/nil", kv(tmap(k, v))),
      #("map/set", kv(types.tfns([tmap(k, v), k, v], tmap(k, v), -1))),
      #("map/rm", kv(types.tfns([tmap(k, v), k], tmap(k, v), -1))),
      #("map/get", kv(types.tfns([tmap(k, v), k], toption(v), -1))),
      #(
        "map/map",
        generic(
          ["k", "v", "v2"],
          types.tfns([types.tfns([v], v2, -1), tmap(k, v)], tmap(k, v2), -1),
        ),
      ),
      #("map/merge", kv(types.tfns([tmap(k, v), tmap(k, v)], tmap(k, v), -1))),
      #("map/values", kv(types.tfns([tmap(k, v)], tlist(v), -1))),
      #("map/keys", kv(types.tfns([tmap(k, v)], tlist(k), -1))),
      #("set/nil", kk(tset(k))),
      #("set/add", kk(types.tfns([tset(k), k], tset(k), -1))),
      #("set/has", kk(types.tfns([tset(k), k], tbool, -1))),
      #("set/rm", kk(types.tfns([tset(k), k], tset(k), -1))),
      #("set/diff", kk(types.tfns([tset(k), tset(k)], tset(k), -1))),
      #("set/merge", kk(types.tfns([tset(k), tset(k)], tset(k), -1))),
      #("set/overlap", kk(types.tfns([tset(k), tset(k)], tset(k), -1))),
      #("set/to-list", kk(types.tfns([tset(k)], tlist(k), -1))),
      #("set/from-list", kk(types.tfns([tlist(k)], tset(k), -1))),
      #("map/from-list", kv(types.tfns([tlist(t_pair(k, v))], tmap(k, v), -1))),
      #("map/to-list", kv(types.tfns([tmap(k, v)], tlist(t_pair(k, v)), -1))),
      #(
        "jsonify",
        generic(["v"], types.tfns([types.Tvar("v", -1)], tstring, -1)),
      ),
      #("valueToString", generic(["v"], types.tfns([vbl("v")], tstring, -1))),
      #(
        "eval",
        generic(["v"], types.tfns([types.Tcon("String", -1)], vbl("v"), -1)),
      ),
      #(
        "eval-with",
        generic(
          ["ctx", "v"],
          types.tfns(
            [types.Tcon("ctx", -1), types.Tcon("String", -1)],
            vbl("v"),
            -1,
          ),
        ),
      ),
      #(
        "errorToString",
        generic(
          ["v"],
          types.tfns(
            [types.tfns([vbl("v")], tstring, -1), vbl("v")],
            tstring,
            -1,
          ),
        ),
      ),
      #("sanitize", concrete(types.tfns([tstring], tstring, -1))),
      #(
        "replace-all",
        concrete(types.tfns([tstring, tstring, tstring], tstring, -1)),
      ),
      #("fatal", generic(["v"], types.tfns([tstring], vbl("v"), -1))),
    ]),
    dict.from_list([
      #("()", #([], [], types.Tcon("()", -1))),
      #(",", #(["a", "b"], [#(option.None, a), #(option.None, b)], t_pair(a, b))),
      #("True", #([], [], tbool)),
      #("False", #([], [], tbool)),
      #("Nil", #([], [], types.Tcon("Nil", -1))),
      #("Error", #(
        ["a", "b"],
        [#(option.None, a)],
        types.Tapp(types.Tcon("Result", -1), [a, b], -1),
      )),
      #("Ok", #(
        ["a", "b"],
        [#(option.None, b)],
        types.Tapp(types.Tcon("Result", -1), [a, b], -1),
      )),
      #("Some", #(
        ["a"],
        [#(option.None, a)],
        types.Tapp(types.Tcon("Option", -1), [a], -1),
      )),
      #("None", #(["a"], [], types.Tapp(types.Tcon("Option", -1), [a], -1))),
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
  )
}
