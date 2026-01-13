import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import typechecker/ast
import typechecker/env
import typechecker/exhaustive
import typechecker/runtime
import typechecker/scheme
import typechecker/state
import typechecker/types

pub fn infer_prim(prim: ast.Prim) -> types.Type {
  case prim {
    ast.Pint(_, loc) -> types.Tcon("int", loc)
    ast.Pfloat(_, loc) -> types.Tcon("float", loc)
    ast.Pbool(_, loc) -> types.Tcon("bool", loc)
  }
}

pub fn infer_quot(quot: ast.Quot, loc: Int) -> types.Type {
  case quot {
    ast.QuotExpr(_) -> types.Tcon("expr", loc)
    ast.QuotTop(_) -> types.Tcon("top", loc)
    ast.QuotType(_) -> types.Tcon("type", loc)
    ast.QuotPat(_) -> types.Tcon("pat", loc)
    ast.QuotQuot(_) -> types.Tcon("cst", loc)
  }
}

pub fn infer_expr(tenv: env.TEnv, expr: ast.Expr) -> state.State(types.Type) {
  use old_subst <- state.bind(state.reset_subst(dict.new()))
  use type_ <- state.bind(infer_expr_inner(tenv, expr))
  use new_subst <- state.bind(state.reset_subst(old_subst))
  use _ignored <- state.bind(state.put_subst(new_subst))
  state.pure(type_)
}

fn infer_expr_inner(tenv: env.TEnv, expr: ast.Expr) -> state.State(types.Type) {
  case expr {
    ast.Evar(name, loc) ->
      case env.resolve(tenv, name) {
        Ok(scheme_) -> instantiate(scheme_, loc)
        Error(_) -> runtime.fatal("Variable not found in scope: " <> name)
      }

    ast.Eprim(prim, _) -> state.pure(infer_prim(prim))

    ast.Equot(quot, loc) -> state.pure(infer_quot(quot, loc))

    ast.Etuple(items, loc) -> {
      use item_types <- state.bind(
        state.map_list(items, fn(item) { infer_expr(tenv, item) }),
      )
      state.pure(types.Ttuple(item_types, loc))
    }

    ast.EtupleIndex(target, index, loc) -> {
      use target_type <- state.bind(infer_expr(tenv, target))
      use applied_target <- state.bind(type_apply_state(target_type))
      case applied_target {
        types.Ttuple(args, _) -> tuple_index_type(args, index, loc)
        types.Tvar(_, _) -> {
          use args <- state.bind(tuple_index_vars(index + 1, loc))
          let tuple_type = types.Ttuple(args, loc)
          use _ignored <- state.bind(unify(applied_target, tuple_type, loc))
          tuple_index_type(args, index, loc)
        }
        _ -> runtime.fatal("Tuple index on non-tuple " <> int.to_string(loc))
      }
    }

    ast.Elist(items, tail, loc) -> {
      use elem_type <- state.bind(new_type_var("list_item", loc))
      let list_type = types.Tapp(types.Tcon("list", loc), elem_type, loc)
      use item_types <- state.bind(
        state.map_list(items, fn(item) { infer_expr(tenv, item) }),
      )
      use _ignored <- state.bind(
        state.each_list(item_types, fn(item_type) {
          unify(item_type, elem_type, loc)
        }),
      )
      case tail {
        None -> type_apply_state(list_type)
        Some(tail_expr) -> {
          use tail_type <- state.bind(infer_expr(tenv, tail_expr))
          use _ignored <- state.bind(unify(tail_type, list_type, loc))
          type_apply_state(list_type)
        }
      }
    }

    ast.Ebitstring(segments, loc) -> {
      use _ignored <- state.bind(
        state.each_list(segments, fn(segment) {
          let #(expr, _opts) = segment
          use _ignored2 <- state.bind(infer_expr(tenv, expr))
          use _ignored3 <- state.bind(infer_bitstring_expr_options(
            tenv,
            segment,
          ))
          state.pure(Nil)
        }),
      )
      state.pure(types.Tcon("bitstring", loc))
    }

    ast.Eecho(value, message, loc) -> {
      use _ignored <- state.bind(case message {
        option.Some(expr) -> {
          use msg_type <- state.bind(infer_expr(tenv, expr))
          use _ignored2 <- state.bind(unify(
            msg_type,
            types.Tcon("string", loc),
            loc,
          ))
          state.pure(Nil)
        }
        option.None -> state.pure(Nil)
      })
      case value {
        option.Some(expr) -> infer_expr(tenv, expr)
        option.None -> state.pure(types.Tcon("()", loc))
      }
    }

    ast.Estr(_, templates, loc) -> {
      use _ignored <- state.bind(
        state.each_list(templates, fn(tuple) {
          let #(expr, _, _) = tuple
          use t <- state.bind(infer_expr(tenv, expr))
          unify(t, types.Tcon("string", loc), loc)
        }),
      )
      state.pure(types.Tcon("string", loc))
    }

    ast.Elambda(args, body, loc) ->
      case args {
        [] -> runtime.fatal("No args to lambda")
        [ast.Pvar(arg, arg_loc)] -> {
          use arg_type <- state.bind(new_type_var(arg, arg_loc))
          let bound_env =
            env.with_type(tenv, arg, scheme.Forall(set.new(), arg_type))
          use body_type <- state.bind(infer_expr(bound_env, body))
          use arg_type_applied <- state.bind(type_apply_state(arg_type))
          state.pure(types.tfn(arg_type_applied, body_type, loc))
        }

        [pat] -> {
          use tuple <- state.bind(infer_pattern(tenv, pat))
          let #(arg_type, scope) = tuple
          use scope_applied <- state.bind(scope_apply_state(scope))
          let bound_env = env.with_scope(tenv, scope_applied)
          use body_type <- state.bind(infer_expr(bound_env, body))
          use arg_type_applied <- state.bind(type_apply_state(arg_type))
          state.pure(types.tfn(arg_type_applied, body_type, loc))
        }

        [one, ..rest] ->
          infer_expr(
            tenv,
            ast.Elambda([one], ast.Elambda(rest, body, loc), loc),
          )
      }

    ast.Eapp(target, args, loc) ->
      case args {
        [] -> infer_expr(tenv, target)
        [arg] -> {
          use result_var <- state.bind(new_type_var("result", loc))
          use target_type <- state.bind(infer_expr_inner(tenv, target))
          use arg_tenv <- state.bind(tenv_apply_state(tenv))
          use arg_type <- state.bind(infer_expr_inner(arg_tenv, arg))
          use target_type_applied <- state.bind(type_apply_state(target_type))
          use _ignored <- state.bind(unify(
            target_type_applied,
            types.tfn(arg_type, result_var, loc),
            loc,
          ))
          type_apply_state(result_var)
        }
        [one, ..rest] ->
          infer_expr(tenv, ast.Eapp(ast.Eapp(target, [one], loc), rest, loc))
      }

    ast.Elet(bindings, body, loc) ->
      case bindings {
        [] -> runtime.fatal("No bindings in let")
        [#(ast.Pvar(name, _), value)] -> {
          use value_type <- state.bind(infer_expr(tenv, value))
          use applied_env <- state.bind(tenv_apply_state(tenv))
          let scheme_ = env.generalize(applied_env, value_type)
          let bound_env = env.with_type(applied_env, name, scheme_)
          infer_expr(bound_env, body)
        }

        [#(pat, value)] -> {
          use tuple <- state.bind(infer_pattern(tenv, pat))
          let #(type_, scope) = tuple
          use value_type <- state.bind(infer_expr(tenv, value))
          use _ignored <- state.bind(unify(type_, value_type, loc))
          use scope_applied <- state.bind(scope_apply_state(scope))
          let bound_env = env.with_scope(tenv, scope_applied)
          infer_expr(bound_env, body)
        }

        [one, ..rest] ->
          infer_expr(tenv, ast.Elet([one], ast.Elet(rest, body, loc), loc))
      }

    ast.Ematch(target, cases, loc) -> {
      use target_type <- state.bind(infer_expr(tenv, target))
      use result_type <- state.bind(new_type_var("match result", loc))
      use result_pair <- state.bind(
        state.foldl_list(cases, #(target_type, result_type), fn(args, args2) {
          let #(target_type_inner, result) = args
          let #(pat, guard, body) = args2
          use args_inner <- state.bind(infer_pattern(tenv, pat))
          let #(type_, scope) = args_inner
          use _ignored <- state.bind(unify(type_, target_type_inner, loc))
          use scope_applied <- state.bind(scope_apply_state(scope))
          let bound_env = env.with_scope(tenv, scope_applied)
          use _ignored_guard <- state.bind(case guard {
            option.Some(expr) -> {
              use guard_type <- state.bind(infer_expr(bound_env, expr))
              unify(guard_type, types.Tcon("bool", loc), loc)
            }
            option.None -> state.pure(Nil)
          })
          use body_type <- state.bind(infer_expr(bound_env, body))
          use subst <- state.bind(state.get_subst())
          use _ignored2 <- state.bind(unify(
            types.type_apply(subst, result),
            body_type,
            loc,
          ))
          use subst2 <- state.bind(state.get_subst())
          let next_target = types.type_apply(subst2, target_type_inner)
          let next_result = types.type_apply(subst2, result)
          state.pure(#(next_target, next_result))
        }),
      )
      let #(_target, final_result) = result_pair
      use target_applied <- state.bind(type_apply_state(target_type))
      case
        list.any(cases, fn(args) {
          let #(_pat, guard, _body) = args
          case guard {
            option.None -> False
            option.Some(_) -> True
          }
        })
      {
        True -> state.pure(Nil)
        False ->
          state.bind(
            exhaustive.check_exhaustiveness(
              tenv,
              target_applied,
              list.map(cases, fn(args) {
                let #(pat, _guard, _body) = args
                pat
              }),
              loc,
            ),
            state.pure,
          )
      }
      state.pure(final_result)
    }
  }
}

pub fn infer_pattern(
  tenv: env.TEnv,
  pattern: ast.Pat,
) -> state.State(#(types.Type, dict.Dict(String, scheme.Scheme))) {
  case pattern {
    ast.Pvar(name, loc) -> {
      use v <- state.bind(new_type_var(name, loc))
      let scope = dict.from_list([#(name, scheme.Forall(set.new(), v))])
      state.pure(#(v, scope))
    }

    ast.Pany(loc) -> {
      use v <- state.bind(new_type_var("any", loc))
      state.pure(#(v, dict.new()))
    }

    ast.Pstr(_, loc) -> state.pure(#(types.Tcon("string", loc), dict.new()))

    ast.Pprim(ast.Pbool(_, _), loc) ->
      state.pure(#(types.Tcon("bool", loc), dict.new()))

    ast.Pprim(ast.Pint(_, _), loc) ->
      state.pure(#(types.Tcon("int", loc), dict.new()))

    ast.Pprim(ast.Pfloat(_, _), loc) ->
      state.pure(#(types.Tcon("float", loc), dict.new()))

    ast.Plist(items, tail, loc) -> {
      use elem_type <- state.bind(new_type_var("list_item", loc))
      let list_type = types.Tapp(types.Tcon("list", loc), elem_type, loc)
      use inferred <- state.bind(
        state.map_list(items, fn(item) { infer_pattern(tenv, item) }),
      )
      let #(item_types, scopes) = list.unzip(inferred)
      use _ignored <- state.bind(
        state.each_list(item_types, fn(item_type) {
          unify(item_type, elem_type, loc)
        }),
      )
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      case tail {
        None -> {
          use list_type_applied <- state.bind(type_apply_state(list_type))
          state.pure(#(list_type_applied, scope))
        }
        Some(tail_pat) -> {
          use tail_tuple <- state.bind(infer_pattern(tenv, tail_pat))
          let #(tail_type, tail_scope) = tail_tuple
          use _ignored <- state.bind(unify(tail_type, list_type, loc))
          let scope = dict.merge(scope, tail_scope)
          use list_type_applied <- state.bind(type_apply_state(list_type))
          state.pure(#(list_type_applied, scope))
        }
      }
    }

    ast.Pas(name, pat, _loc) -> {
      use tuple <- state.bind(infer_pattern(tenv, pat))
      let #(type_, scope) = tuple
      let scope = dict.insert(scope, name, scheme.Forall(set.new(), type_))
      state.pure(#(type_, scope))
    }

    ast.Pconcat(_prefix, prefix_name, rest_name, loc) -> {
      let scope =
        dict.merge(
          case prefix_name {
            option.None -> dict.new()
            option.Some(name) ->
              dict.from_list([
                #(name, scheme.Forall(set.new(), types.Tcon("string", loc))),
              ])
          },
          case rest_name {
            option.None -> dict.new()
            option.Some(name) ->
              dict.from_list([
                #(name, scheme.Forall(set.new(), types.Tcon("string", loc))),
              ])
          },
        )
      state.pure(#(types.Tcon("string", loc), scope))
    }

    ast.Pbitstring(segments, loc) -> {
      use scopes <- state.bind(
        state.map_list(segments, fn(segment) {
          infer_bitstring_pat_segment(tenv, segment)
        }),
      )
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      state.pure(#(types.Tcon("bitstring", loc), scope))
    }

    ast.Ptuple(items, loc) -> {
      use inferred <- state.bind(
        state.map_list(items, fn(item) { infer_pattern(tenv, item) }),
      )
      let #(types_, scopes) = list.unzip(inferred)
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      state.pure(#(types.Ttuple(types_, loc), scope))
    }

    ast.Pcon(name, _name_loc, args, loc) -> {
      use args2 <- state.bind(instantiate_tcon(tenv, name, loc))
      let #(cargs, cres) = args2
      use sub_patterns <- state.bind(
        state.map_list(args, fn(arg) { infer_pattern(tenv, arg) }),
      )
      let #(arg_types, scopes) = list.unzip(sub_patterns)
      use _ignored <- state.bind(
        state.each_list(list.zip(arg_types, cargs), fn(args_pair) {
          let #(ptype, ctype) = args_pair
          unify(ptype, ctype, loc)
        }),
      )
      use cres_applied <- state.bind(type_apply_state(cres))
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      state.pure(#(cres_applied, scope))
    }
  }
}

pub fn instantiate_tcon(
  tenv: env.TEnv,
  name: String,
  loc: Int,
) -> state.State(#(List(types.Type), types.Type)) {
  let env.TEnv(_values, tcons, _types, _aliases) = tenv
  case dict.get(tcons, name) {
    Error(_) -> runtime.fatal("Unknown type constructor: " <> name)
    Ok(#(free, cargs, cres)) -> {
      use subst <- state.bind(make_subst_for_free(set.from_list(free), loc))
      let args = list.map(cargs, fn(t) { types.type_apply(subst, t) })
      let res = types.type_apply(subst, cres)
      state.pure(#(args, res))
    }
  }
}

pub fn new_type_var(name: String, loc: Int) -> state.State(types.Type) {
  use idx <- state.bind(state.next_idx())
  state.pure(types.Tvar(name <> ":" <> int.to_string(idx), loc))
}

pub fn make_subst_for_free(
  vars: set.Set(String),
  loc: Int,
) -> state.State(types.Subst) {
  use mapping <- state.bind(
    state.map_list(set.to_list(vars), fn(id) {
      use new_var <- state.bind(new_type_var(id, loc))
      state.pure(#(id, new_var))
    }),
  )
  state.pure(dict.from_list(mapping))
}

pub fn instantiate(scheme_: scheme.Scheme, loc: Int) -> state.State(types.Type) {
  let scheme.Forall(vars, type_) = scheme_
  use subst <- state.bind(make_subst_for_free(vars, loc))
  state.pure(types.type_apply(subst, type_))
}

pub fn unify(t1: types.Type, t2: types.Type, loc: Int) -> state.State(Nil) {
  case t1, t2 {
    types.Tvar(var, _), t -> var_bind(var, t, loc)
    t, types.Tvar(var, _) -> var_bind(var, t, loc)
    types.Tcon(a, _), types.Tcon(b, _) ->
      case a == b {
        True -> state.pure(Nil)
        False ->
          runtime.fatal(
            "Incompatible concrete types: "
            <> a
            <> " ("
            <> int.to_string(loc)
            <> ") vs "
            <> b
            <> " ("
            <> int.to_string(loc)
            <> ")",
          )
      }
    types.Tapp(t1a, a1, _), types.Tapp(t2a, a2, _) -> {
      use _ignored <- state.bind(unify(t1a, t2a, loc))
      use subst <- state.bind(state.get_subst())
      let left = types.type_apply(subst, a1)
      let right = types.type_apply(subst, a2)
      unify(left, right, loc)
    }
    types.Ttuple(args1, _), types.Ttuple(args2, _) ->
      case list.length(args1) == list.length(args2) {
        True ->
          state.each_list(list.zip(args1, args2), fn(args) {
            let #(left, right) = args
            unify(left, right, loc)
          })
        False ->
          runtime.fatal(
            "Incompatible tuple arity "
            <> int.to_string(loc)
            <> " "
            <> runtime.jsonify(t1)
            <> " (vs) "
            <> runtime.jsonify(t2),
          )
      }
    _, _ ->
      runtime.fatal(
        "Incompatible types: "
        <> runtime.jsonify(t1)
        <> " (vs) "
        <> runtime.jsonify(t2),
      )
  }
}

pub fn var_bind(var: String, type_: types.Type, _loc: Int) -> state.State(Nil) {
  case type_ {
    types.Tvar(v, _) ->
      case var == v {
        True -> state.pure(Nil)
        False -> {
          use _ignored <- state.bind(state.put_subst(one_subst(var, type_)))
          state.pure(Nil)
        }
      }
    _ ->
      case set.contains(types.type_free(type_), var) {
        True ->
          runtime.fatal(
            "Cycle found while unifying type with type variable. " <> var,
          )
        False -> {
          use _ignored <- state.bind(state.put_subst(one_subst(var, type_)))
          state.pure(Nil)
        }
      }
  }
}

pub fn one_subst(var: String, type_: types.Type) -> types.Subst {
  dict.from_list([#(var, type_)])
}

fn tenv_apply_state(tenv: env.TEnv) -> state.State(env.TEnv) {
  state.apply_with(env.apply, tenv)
}

fn type_apply_state(type_: types.Type) -> state.State(types.Type) {
  state.apply_with(types.type_apply, type_)
}

fn scope_apply_state(
  scope: dict.Dict(String, scheme.Scheme),
) -> state.State(dict.Dict(String, scheme.Scheme)) {
  state.apply_with(env.scope_apply, scope)
}

fn tuple_index_type(
  args: List(types.Type),
  index: Int,
  loc: Int,
) -> state.State(types.Type) {
  case args, index {
    [], _ -> runtime.fatal("Tuple index out of range " <> int.to_string(loc))
    [head, ..], 0 -> type_apply_state(head)
    [_head, ..tail], _ -> tuple_index_type(tail, index - 1, loc)
  }
}

fn tuple_index_vars(count: Int, loc: Int) -> state.State(List(types.Type)) {
  case count <= 0 {
    True -> state.pure([])
    False -> {
      use rest <- state.bind(tuple_index_vars(count - 1, loc))
      use v <- state.bind(new_type_var("tuple_item", loc))
      state.pure([v, ..rest])
    }
  }
}

fn infer_bitstring_expr_options(
  tenv: env.TEnv,
  segment: #(ast.Expr, List(ast.BitStringSegmentOption(ast.Expr))),
) -> state.State(Nil) {
  let #(_expr, options) = segment
  state.each_list(options, fn(opt) {
    case opt {
      ast.SizeValueOption(expr) -> {
        use _ignored <- state.bind(infer_expr(tenv, expr))
        state.pure(Nil)
      }
      _ -> state.pure(Nil)
    }
  })
}

fn infer_bitstring_pat_segment(
  tenv: env.TEnv,
  segment: #(ast.Pat, List(ast.BitStringSegmentOption(ast.Pat))),
) -> state.State(dict.Dict(String, scheme.Scheme)) {
  let #(pat, options) = segment
  use tuple <- state.bind(infer_pattern(tenv, pat))
  let #(_type, scope) = tuple
  use option_scope <- state.bind(infer_bitstring_pat_options(tenv, options))
  state.pure(dict.merge(scope, option_scope))
}

fn infer_bitstring_pat_options(
  tenv: env.TEnv,
  options: List(ast.BitStringSegmentOption(ast.Pat)),
) -> state.State(dict.Dict(String, scheme.Scheme)) {
  state.foldl_list(options, dict.new(), fn(acc, opt) {
    case opt {
      ast.SizeValueOption(pat) -> {
        use tuple <- state.bind(infer_pattern(tenv, pat))
        let #(_type, scope) = tuple
        state.pure(dict.merge(acc, scope))
      }
      _ -> state.pure(acc)
    }
  })
}
