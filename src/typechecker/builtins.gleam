import gleam/dict
import gleam/list
import gleam/option
import gleam/set
import typechecker/ast
import typechecker/env
import typechecker/infer
import typechecker/scheme
import typechecker/state
import typechecker/types

pub fn add_def(
  tenv: env.TEnv,
  name: String,
  name_loc: Int,
  expr: ast.Expr,
  loc: Int,
) -> env.TEnv {
  state.run_empty({
    use self <- state.bind(infer.new_type_var(name, name_loc))
    let bound_env = env.with_type(tenv, name, scheme.Forall(set.new(), self))
    use type_ <- state.bind(infer.infer_expr(bound_env, expr))
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
  defns: List(#(String, Int, ast.Expr, Int)),
) -> env.TEnv {
  let names =
    list.map(defns, fn(args) {
      let #(name, _, _, _) = args
      name
    })
  let locs =
    list.map(defns, fn(args) {
      let #(_, _, _, loc) = args
      loc
    })

  state.run_empty({
    use vbls <- state.bind(
      state.map_list(defns, fn(args) {
        let #(name, name_loc, _expr, _loc) = args
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
        let #(_, _, expr, _) = args
        infer.infer_expr(bound_env, expr)
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
  let res =
    list.fold(args, types.Tcon(name, loc), fn(inner, args) {
      let #(arg_name, arg_loc) = args
      types.Tapp(inner, types.Tvar(arg_name, arg_loc), loc)
    })

  let env.TEnv(_values, _tcons, _types, aliases) = tenv

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

  env.TEnv(values, tcons, types_map, dict.new())
}

pub fn add_stmt(tenv: env.TEnv, stmt: ast.Top) -> env.TEnv {
  case stmt {
    ast.Tdef(name, name_loc, expr, loc) ->
      add_def(tenv, name, name_loc, expr, loc)
    ast.Texpr(expr, _loc) ->
      state.run_empty({
        use _ignored <- state.bind(infer.infer_expr(tenv, expr))
        state.pure(env.empty())
      })
    ast.Ttypealias(name, _name_loc, args, type_, _loc) ->
      add_typealias(tenv, name, args, type_)
    ast.Tdeftype(name, _name_loc, args, constrs, loc) ->
      add_deftype(tenv, name, args, constrs, loc)
  }
}

pub fn split_stmts(
  stmts: List(ast.Top),
) -> #(List(#(String, Int, ast.Expr, Int)), List(ast.Top), List(ast.Top)) {
  list.fold(stmts, #([], [], []), fn(acc, stmt) {
    let #(defs, aliases, others) = acc
    case stmt {
      ast.Tdef(name, name_loc, body, loc) -> #(
        [#(name, name_loc, body, loc), ..defs],
        aliases,
        others,
      )
      ast.Ttypealias(_, _, _, _, _) -> #(defs, [stmt, ..aliases], others)
      _ -> #(defs, aliases, [stmt, ..others])
    }
  })
}

pub fn add_stmts(tenv: env.TEnv, stmts: List(ast.Top)) -> env.TEnv {
  let #(defs, aliases, others) = split_stmts(stmts)
  let def_groups = group_mutual_defs(defs)
  let denv =
    list.fold(def_groups, env.empty(), fn(acc, group) {
      let merged = env.merge(tenv, acc)
      env.merge(acc, add_defs(merged, group))
    })
  let folded =
    list.fold(list.append(aliases, others), denv, fn(acc, stmt) {
      env.merge(acc, add_stmt(env.merge(tenv, acc), stmt))
    })
  folded
}

type DefInfo =
  #(String, Int, ast.Expr, Int, Int)

fn group_mutual_defs(
  defs: List(#(String, Int, ast.Expr, Int)),
) -> List(List(#(String, Int, ast.Expr, Int))) {
  let defs_in_order = list.reverse(defs)
  let infos = def_infos(defs_in_order)
  let groups = def_info_groups(infos)
  list.map(groups, fn(group) {
    list.map(group, fn(info) {
      let #(name, name_loc, expr, loc, _idx) = info
      #(name, name_loc, expr, loc)
    })
  })
}

fn def_infos(defs: List(#(String, Int, ast.Expr, Int))) -> List(DefInfo) {
  let #(_idx, infos_rev) =
    list.fold(defs, #(0, []), fn(acc, def) {
      let #(idx, infos) = acc
      let #(name, name_loc, expr, loc) = def
      #(idx + 1, [#(name, name_loc, expr, loc, idx), ..infos])
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
  let #(name, _, _, _, _) = def
  name
}

fn def_expr(def: DefInfo) -> ast.Expr {
  let #(_, _, expr, _, _) = def
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

fn expr_free(expr: ast.Expr, bound: set.Set(String)) -> set.Set(String) {
  case expr {
    ast.Eprim(_, _) -> set.new()
    ast.Evar(name, _) ->
      case set.contains(bound, name) {
        True -> set.new()
        False -> set.insert(set.new(), name)
      }
    ast.Estr(_, templates, _) ->
      list.fold(templates, set.new(), fn(acc, item) {
        let #(expr, _, _) = item
        set.union(acc, expr_free(expr, bound))
      })
    ast.Equot(_, _) -> set.new()
    ast.Etuple(items, _) ->
      list.fold(items, set.new(), fn(acc, item) {
        set.union(acc, expr_free(item, bound))
      })
    ast.EtupleIndex(target, _index, _) -> expr_free(target, bound)
    ast.Elist(items, tail, _) -> {
      let items_set =
        list.fold(items, set.new(), fn(acc, item) {
          set.union(acc, expr_free(item, bound))
        })
      case tail {
        option.None -> items_set
        option.Some(expr) -> set.union(items_set, expr_free(expr, bound))
      }
    }
    ast.Ebitstring(segments, _) ->
      list.fold(segments, set.new(), fn(acc, segment) {
        let #(expr, _opts) = segment
        set.union(acc, expr_free(expr, bound))
      })
    ast.Eecho(value, message, _) -> {
      let acc = case value {
        option.None -> set.new()
        option.Some(expr) -> expr_free(expr, bound)
      }
      case message {
        option.None -> acc
        option.Some(expr) -> set.union(acc, expr_free(expr, bound))
      }
    }
    ast.Erecord(_module, _name, fields, _) ->
      list.fold(fields, set.new(), fn(acc, field) {
        let #(_label, expr) = field
        set.union(acc, expr_free(expr, bound))
      })
    ast.ErecordUpdate(_module, _name, record, fields, _) -> {
      let record_free = expr_free(record, bound)
      list.fold(fields, record_free, fn(acc, field) {
        let #(_label, expr) = field
        set.union(acc, expr_free(expr, bound))
      })
    }
    ast.Efield(expr, _label, _) -> expr_free(expr, bound)
    ast.Elambda(args, body, _) -> {
      let bound_args =
        list.fold(args, set.new(), fn(acc, pat) {
          set.union(acc, pat_bound(pat))
        })
      expr_free(body, set.union(bound, bound_args))
    }
    ast.Eapp(target, args, _) -> {
      let target_free = expr_free(target, bound)
      list.fold(args, target_free, fn(acc, arg) {
        set.union(acc, expr_free(arg, bound))
      })
    }
    ast.Elet(bindings, body, _) -> {
      let #(bound2, free) =
        list.fold(bindings, #(bound, set.new()), fn(acc, binding) {
          let #(bound_now, free_now) = acc
          let #(pat, value) = binding
          let free_value = expr_free(value, bound_now)
          let bound_next = set.union(bound_now, pat_bound(pat))
          #(bound_next, set.union(free_now, free_value))
        })
      set.union(free, expr_free(body, bound2))
    }
    ast.Ematch(target, cases, _) -> {
      let target_free = expr_free(target, bound)
      let cases_free =
        list.fold(cases, set.new(), fn(acc, item) {
          let #(pat, guard, body) = item
          let bound_case = set.union(bound, pat_bound(pat))
          let guard_free = case guard {
            option.None -> set.new()
            option.Some(expr) -> expr_free(expr, bound_case)
          }
          let body_free = expr_free(body, bound_case)
          set.union(acc, set.union(guard_free, body_free))
        })
      set.union(target_free, cases_free)
    }
  }
}

fn pat_bound(pat: ast.Pat) -> set.Set(String) {
  case pat {
    ast.Pany(_) -> set.new()
    ast.Pvar(name, _) -> set.insert(set.new(), name)
    ast.Ptuple(items, _) ->
      list.fold(items, set.new(), fn(acc, item) {
        set.union(acc, pat_bound(item))
      })
    ast.Plist(items, tail, _) -> {
      let items_set =
        list.fold(items, set.new(), fn(acc, item) {
          set.union(acc, pat_bound(item))
        })
      case tail {
        option.None -> items_set
        option.Some(pat) -> set.union(items_set, pat_bound(pat))
      }
    }
    ast.Pas(name, pat, _) -> set.insert(pat_bound(pat), name)
    ast.Pconcat(_prefix, prefix_name, rest_name, _) -> {
      let acc = case prefix_name {
        option.None -> set.new()
        option.Some(name) -> set.insert(set.new(), name)
      }
      case rest_name {
        option.None -> acc
        option.Some(name) -> set.insert(acc, name)
      }
    }
    ast.Pbitstring(segments, _) ->
      list.fold(segments, set.new(), fn(acc, segment) {
        let #(pat, _opts) = segment
        set.union(acc, pat_bound(pat))
      })
    ast.Pcon(_, _, args, _) ->
      list.fold(args, set.new(), fn(acc, arg) {
        let #(_label, pat) = arg
        set.union(acc, pat_bound(pat))
      })
    ast.Pstr(_, _) -> set.new()
    ast.Pprim(_, _) -> set.new()
  }
}

pub const tbool: types.Type = types.Tcon("bool", -1)

pub fn tmap(k: types.Type, v: types.Type) -> types.Type {
  types.Tapp(types.Tapp(types.Tcon("map", -1), k, -1), v, -1)
}

pub fn toption(arg: types.Type) -> types.Type {
  types.Tapp(types.Tcon("option", -1), arg, -1)
}

pub fn tlist(arg: types.Type) -> types.Type {
  types.Tapp(types.Tcon("list", -1), arg, -1)
}

pub fn tset(arg: types.Type) -> types.Type {
  types.Tapp(types.Tcon("set", -1), arg, -1)
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

pub const tstring: types.Type = types.Tcon("string", -1)

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
              types.Tcon("list", -1),
              types.Tapp(types.Tcon("trace-fmt", -1), k, -1),
              -1,
            ),
          ],
          types.Tcon("()", -1),
          -1,
        )),
      ),
      #("unescapeString", concrete(types.tfns([tstring], tstring, -1))),
      #("int-to-string", concrete(types.tfns([types.tint], tstring, -1))),
      #(
        "string-to-int",
        concrete(types.tfns([tstring], toption(types.tint), -1)),
      ),
      #(
        "string-to-float",
        concrete(types.tfns([tstring], toption(types.Tcon("float", -1)), -1)),
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
        generic(["v"], types.tfns([types.Tcon("string", -1)], vbl("v"), -1)),
      ),
      #(
        "eval-with",
        generic(
          ["ctx", "v"],
          types.tfns(
            [types.Tcon("ctx", -1), types.Tcon("string", -1)],
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
    ]),
    dict.from_list([
      #("int", #(0, set.new())),
      #("float", #(0, set.new())),
      #("string", #(0, set.new())),
      #("bool", #(0, set.new())),
      #("bitstring", #(0, set.new())),
      #("list", #(1, set.new())),
      #("map", #(2, set.new())),
      #("set", #(1, set.new())),
      #("->", #(2, set.new())),
    ]),
    dict.new(),
  )
}
