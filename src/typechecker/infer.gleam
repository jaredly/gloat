import gleam/dict
import gleam/list
import gleam/set
import gleam/string
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

pub fn infer_quot(_quot: ast.Quot, loc: Int) -> types.Type {
  case _quot {
    ast.QuotExpr(_) -> types.Tcon("expr", loc)
    ast.QuotTop(_) -> types.Tcon("top", loc)
    ast.QuotType(_) -> types.Tcon("type", loc)
    ast.QuotPat(_) -> types.Tcon("pat", loc)
    ast.QuotQuot(_) -> types.Tcon("cst", loc)
  }
}

pub fn infer_expr(tenv: env.TEnv, expr: ast.Expr) -> state.State(types.Type) {
  state.bind(state.reset_subst(dict.new()), fn(old_subst) {
    state.bind(infer_expr_inner(tenv, expr), fn(type_) {
      state.bind(state.reset_subst(old_subst), fn(new_subst) {
        state.bind(state.put_subst(new_subst), fn(_) { state.pure(type_) })
      })
    })
  })
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

    ast.Estr(_, templates, loc) ->
      state.bind(
        state.each_list(templates, fn(#(expr, _, _)) {
          state.bind(infer_expr(tenv, expr), fn(t) {
            unify(t, types.Tcon("string", loc), loc)
          })
        }),
        fn(_) { state.pure(types.Tcon("string", loc)) },
      )

    ast.Elambda(args, body, loc) ->
      case args {
        [] -> runtime.fatal("No args to lambda")
        [ast.Pvar(arg, arg_loc)] ->
          state.bind(new_type_var(arg, arg_loc), fn(arg_type) {
            let bound_env = env.with_type(tenv, arg, scheme.Forall(set.new(), arg_type))
            state.bind(infer_expr(bound_env, body), fn(body_type) {
              state.bind(type_apply_state(arg_type), fn(arg_type_applied) {
                state.pure(types.tfn(arg_type_applied, body_type, loc))
              })
            })
          })

        [pat] ->
          state.bind(infer_pattern(tenv, pat), fn(#(arg_type, scope)) {
            state.bind(scope_apply_state(scope), fn(scope_applied) {
              let bound_env = env.with_scope(tenv, scope_applied)
              state.bind(infer_expr(bound_env, body), fn(body_type) {
                state.bind(type_apply_state(arg_type), fn(arg_type_applied) {
                  state.pure(types.tfn(arg_type_applied, body_type, loc))
                })
              })
            })
          })

        [one, ..rest] ->
          infer_expr(tenv, ast.Elambda([one], ast.Elambda(rest, body, loc), loc))
      }

    ast.Eapp(target, args, loc) ->
      case args {
        [] -> infer_expr(tenv, target)
        [arg] ->
          state.bind(new_type_var("result", loc), fn(result_var) {
            state.bind(infer_expr(tenv, target), fn(target_type) {
              state.bind(tenv_apply_state(tenv), fn(arg_tenv) {
                state.bind(infer_expr(arg_tenv, arg), fn(arg_type) {
                  state.bind(type_apply_state(target_type), fn(target_type_applied) {
                    state.bind(
                      unify(target_type_applied, types.tfn(arg_type, result_var, loc), loc),
                      fn(_) { type_apply_state(result_var) },
                    )
                  })
                })
              })
            })
          })
        [one, ..rest] ->
          infer_expr(tenv, ast.Eapp(ast.Eapp(target, [one], loc), rest, loc))
      }

    ast.Elet(bindings, body, loc) ->
      case bindings {
        [] -> runtime.fatal("No bindings in let")
        [#(ast.Pvar(name, _), value)] ->
          state.bind(infer_expr(tenv, value), fn(value_type) {
            state.bind(tenv_apply_state(tenv), fn(applied_env) {
              let scheme_ = env.generalize(applied_env, value_type)
              let bound_env = env.with_type(applied_env, name, scheme_)
              infer_expr(bound_env, body)
            })
          })

        [#(pat, value)] ->
          state.bind(infer_pattern(tenv, pat), fn(#(type_, scope)) {
            state.bind(infer_expr(tenv, value), fn(value_type) {
              state.bind(unify(type_, value_type, loc), fn(_) {
                state.bind(scope_apply_state(scope), fn(scope_applied) {
                  let bound_env = env.with_scope(tenv, scope_applied)
                  infer_expr(bound_env, body)
                })
              })
            })
          })

        [one, ..rest] -> infer_expr(tenv, ast.Elet([one], ast.Elet(rest, body, loc), loc))
      }

    ast.Ematch(target, cases, loc) ->
      state.bind(infer_expr(tenv, target), fn(target_type) {
        state.bind(new_type_var("match result", loc), fn(result_type) {
          state.bind(
            state.foldl_list(
              cases,
              #(target_type, result_type),
              fn(#(target_type_inner, result), #(pat, body)) {
                state.bind(infer_pattern(tenv, pat), fn(#(type_, scope)) {
                  state.bind(unify(type_, target_type_inner, loc), fn(_) {
                    state.bind(scope_apply_state(scope), fn(scope_applied) {
                      let bound_env = env.with_scope(tenv, scope_applied)
                      state.bind(infer_expr(bound_env, body), fn(body_type) {
                        state.bind(state.get_subst(), fn(subst) {
                          state.bind(
                            unify(
                              types.type_apply(subst, result),
                              body_type,
                              loc,
                            ),
                            fn(_) {
                              state.bind(state.get_subst(), fn(subst2) {
                                let next_target = types.type_apply(subst2, target_type_inner)
                                let next_result = types.type_apply(subst2, result)
                                state.pure(#(next_target, next_result))
                              })
                            },
                          )
                        })
                      })
                    })
                  })
                })
              },
            ),
            fn(#(_target, final_result)) {
              state.bind(type_apply_state(target_type), fn(target_applied) {
                state.bind(
                  exhaustive.check_exhaustiveness(tenv, target_applied, list.map(cases, fn(#(pat, _)) { pat }), loc),
                  fn(_) { state.pure(final_result) },
                )
              })
            },
          )
        })
      })
  }
}

pub fn infer_pattern(tenv: env.TEnv, pattern: ast.Pat) ->
  state.State(#(types.Type, dict.Dict(String, scheme.Scheme))) {
  case pattern {
    ast.Pvar(name, loc) ->
      state.bind(new_type_var(name, loc), fn(v) {
        let scope = dict.from_list([#(name, scheme.Forall(set.new(), v))])
        state.pure(#(v, scope))
      })

    ast.Pany(loc) -> state.bind(new_type_var("any", loc), fn(v) { state.pure(#(v, dict.new())) })

    ast.Pstr(_, loc) -> state.pure(#(types.Tcon("string", loc), dict.new()))

    ast.Pprim(ast.Pbool(_, _), loc) -> state.pure(#(types.Tcon("bool", loc), dict.new()))

    ast.Pprim(ast.Pint(_, _), loc) -> state.pure(#(types.Tcon("int", loc), dict.new()))

    ast.Pcon(name, _name_loc, args, loc) ->
      state.bind(instantiate_tcon(tenv, name, loc), fn(#(cargs, cres)) {
        state.bind(state.map_list(args, fn(arg) { infer_pattern(tenv, arg) }), fn(sub_patterns) {
          let #(arg_types, scopes) = list.unzip(sub_patterns)
          state.bind(
            state.each_list(list.zip(arg_types, cargs), fn(#(ptype, ctype)) { unify(ptype, ctype, loc) }),
            fn(_) {
              state.bind(type_apply_state(cres), fn(cres_applied) {
                let scope = list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
                state.pure(#(cres_applied, scope))
              })
            },
          )
        })
      })
  }
}

pub fn instantiate_tcon(tenv: env.TEnv, name: String, loc: Int) ->
  state.State(#(List(types.Type), types.Type)) {
  let env.TEnv(_values, tcons, _types, _aliases) = tenv
  case dict.get(tcons, name) {
    Error(_) -> runtime.fatal("Unknown type constructor: " <> name)
    Ok(#(free, cargs, cres)) ->
      state.bind(make_subst_for_free(set.from_list(free), loc), fn(subst) {
        let args = list.map(cargs, fn(t) { types.type_apply(subst, t) })
        let res = types.type_apply(subst, cres)
        state.pure(#(args, res))
      })
  }
}

pub fn new_type_var(name: String, loc: Int) -> state.State(types.Type) {
  state.bind(state.next_idx(), fn(idx) {
    state.pure(types.Tvar(name <> ":" <> string.from_int(idx), loc))
  })
}

pub fn make_subst_for_free(vars: set.Set(String), loc: Int) -> state.State(types.Subst) {
  state.bind(
    state.map_list(set.to_list(vars), fn(id) {
      state.bind(new_type_var(id, loc), fn(new_var) { state.pure(#(id, new_var)) })
    }),
    fn(mapping) { state.pure(dict.from_list(mapping)) },
  )
}

pub fn instantiate(scheme_: scheme.Scheme, loc: Int) -> state.State(types.Type) {
  let scheme.Forall(vars, type_) = scheme_
  state.bind(make_subst_for_free(vars, loc), fn(subst) { state.pure(types.type_apply(subst, type_)) })
}

pub fn unify(t1: types.Type, t2: types.Type, loc: Int) -> state.State(Nil) {
  case #(t1, t2) {
    #(types.Tvar(var, _), t) -> var_bind(var, t, loc)
    #(t, types.Tvar(var, _)) -> var_bind(var, t, loc)
    #(types.Tcon(a, _), types.Tcon(b, _)) ->
      case a == b {
        True -> state.pure(Nil)
        False ->
          runtime.fatal(
            "Incompatible concrete types: "
              <> a
              <> " ("
              <> string.from_int(loc)
              <> ") vs "
              <> b
              <> " ("
              <> string.from_int(loc)
              <> ")",
          )
      }
    #(types.Tapp(t1a, a1, _), types.Tapp(t2a, a2, _)) ->
      state.bind(unify(t1a, t2a, loc), fn(_) {
        state.bind(state.get_subst(), fn(subst) {
          let left = types.type_apply(subst, a1)
          let right = types.type_apply(subst, a2)
          unify(left, right, loc)
        })
      })
    _ -> runtime.fatal("Incompatible types: " <> runtime.jsonify(t1) <> " " <> runtime.jsonify(t2))
  }
}

pub fn var_bind(var: String, type_: types.Type, _loc: Int) -> state.State(Nil) {
  case type_ {
    types.Tvar(v, _) ->
      case var == v {
        True -> state.pure(Nil)
        False -> state.bind(state.put_subst(one_subst(var, type_)), fn(_) { state.pure(Nil) })
      }
    _ ->
      case set.contains(types.type_free(type_), var) {
        True -> runtime.fatal("Cycle found while unifying type with type variable. " <> var)
        False -> state.bind(state.put_subst(one_subst(var, type_)), fn(_) { state.pure(Nil) })
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

fn scope_apply_state(scope: dict.Dict(String, scheme.Scheme)) ->
  state.State(dict.Dict(String, scheme.Scheme)) {
  state.apply_with(env.scope_apply, scope)
}
