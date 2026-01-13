import gleam/dict
import gleam/int
import gleam/list
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
          use target_type <- state.bind(infer_expr(tenv, target))
          use arg_tenv <- state.bind(tenv_apply_state(tenv))
          use arg_type <- state.bind(infer_expr(arg_tenv, arg))
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
          let #(pat, body) = args2
          use args_inner <- state.bind(infer_pattern(tenv, pat))
          let #(type_, scope) = args_inner
          use _ignored <- state.bind(unify(type_, target_type_inner, loc))
          use scope_applied <- state.bind(scope_apply_state(scope))
          let bound_env = env.with_scope(tenv, scope_applied)
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
      use _ignored <- state.bind(exhaustive.check_exhaustiveness(
        tenv,
        target_applied,
        list.map(cases, fn(args) {
          let #(pat, _) = args
          pat
        }),
        loc,
      ))
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
