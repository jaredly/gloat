import glance as g
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/set
import typechecker/env
import typechecker/exhaustive
import typechecker/gleam_types
import typechecker/runtime
import typechecker/scheme
import typechecker/state
import typechecker/types

pub fn infer_expr(tenv: env.TEnv, expr: g.Expression) -> state.State(types.Type) {
  use old_subst <- state.bind(state.reset_subst(dict.new()))
  use type_ <- state.bind(infer_expr_inner(tenv, expr))
  use new_subst <- state.bind(state.reset_subst(old_subst))
  use _ignored <- state.bind(state.put_subst(new_subst))
  state.pure(type_)
}

fn infer_expr_inner(
  tenv: env.TEnv,
  expr: g.Expression,
) -> state.State(types.Type) {
  case expr {
    g.Int(span, value) ->
      case int.parse(value) {
        Ok(_) -> state.pure(types.Tcon("int", gleam_types.loc_from_span(span)))
        Error(_) ->
          runtime.fatal(
            "Invalid int literal "
            <> int.to_string(gleam_types.loc_from_span(span)),
          )
      }

    g.Float(span, value) ->
      case float.parse(value) {
        Ok(_) ->
          state.pure(types.Tcon("float", gleam_types.loc_from_span(span)))
        Error(_) ->
          runtime.fatal(
            "Invalid float literal "
            <> int.to_string(gleam_types.loc_from_span(span)),
          )
      }

    g.String(span, _value) ->
      state.pure(types.Tcon("string", gleam_types.loc_from_span(span)))

    g.Variable(span, name) ->
      case name {
        "True" ->
          state.pure(types.Tcon("bool", gleam_types.loc_from_span(span)))
        "False" ->
          state.pure(types.Tcon("bool", gleam_types.loc_from_span(span)))
        _ ->
          case env.resolve(tenv, name) {
            Ok(scheme_) -> instantiate(scheme_, gleam_types.loc_from_span(span))
            Error(_) -> runtime.fatal("Variable not found in scope: " <> name)
          }
      }

    g.NegateInt(span, value) -> {
      let loc = gleam_types.loc_from_span(span)
      use value_type <- state.bind(infer_expr(tenv, value))
      use _ignored <- state.bind(unify(value_type, types.Tcon("int", loc), loc))
      state.pure(types.Tcon("int", loc))
    }

    g.NegateBool(span, value) -> {
      let loc = gleam_types.loc_from_span(span)
      use value_type <- state.bind(infer_expr(tenv, value))
      use _ignored <- state.bind(unify(value_type, types.Tcon("bool", loc), loc))
      state.pure(types.Tcon("bool", loc))
    }

    g.Block(span, statements) ->
      infer_block(tenv, statements, gleam_types.loc_from_span(span))

    g.Panic(span, message) -> {
      let loc = gleam_types.loc_from_span(span)
      use _ignored <- state.bind(case message {
        option.Some(expr) -> {
          use msg_type <- state.bind(infer_expr(tenv, expr))
          unify(msg_type, types.Tcon("string", loc), loc)
        }
        option.None -> state.pure(Nil)
      })
      new_type_var("panic", loc)
    }

    g.Todo(span, message) -> {
      let loc = gleam_types.loc_from_span(span)
      use _ignored <- state.bind(case message {
        option.Some(expr) -> {
          use msg_type <- state.bind(infer_expr(tenv, expr))
          unify(msg_type, types.Tcon("string", loc), loc)
        }
        option.None -> state.pure(Nil)
      })
      new_type_var("todo", loc)
    }

    g.Tuple(span, items) -> {
      use item_types <- state.bind(
        state.map_list(items, fn(item) { infer_expr(tenv, item) }),
      )
      state.pure(types.Ttuple(item_types, gleam_types.loc_from_span(span)))
    }

    g.TupleIndex(span, target, index) -> {
      let loc = gleam_types.loc_from_span(span)
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

    g.List(span, items, tail) -> {
      let loc = gleam_types.loc_from_span(span)
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
        option.None -> type_apply_state(list_type)
        option.Some(tail_expr) -> {
          use tail_type <- state.bind(infer_expr(tenv, tail_expr))
          use _ignored <- state.bind(unify(tail_type, list_type, loc))
          type_apply_state(list_type)
        }
      }
    }

    g.BitString(span, segments) -> {
      let loc = gleam_types.loc_from_span(span)
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

    g.Echo(span, value, message) -> {
      let loc = gleam_types.loc_from_span(span)
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

    g.RecordUpdate(span, _module, name, record, fields) -> {
      let loc = gleam_types.loc_from_span(span)
      use args2 <- state.bind(instantiate_tcon(tenv, name, loc))
      let #(cfields, cres) = args2
      use record_type <- state.bind(infer_expr(tenv, record))
      use _ignored <- state.bind(unify(record_type, cres, loc))
      use _ignored2 <- state.bind(
        state.each_list(fields, fn(field) {
          let g.RecordUpdateField(label, item) = field
          let ctype = lookup_constructor_field(label, cfields, loc)
          let expr = case item {
            option.Some(expr) -> expr
            option.None -> g.Variable(span, label)
          }
          use expr_type <- state.bind(infer_expr(tenv, expr))
          unify(expr_type, ctype, loc)
        }),
      )
      type_apply_state(cres)
    }

    g.FieldAccess(span, container, label) -> {
      let loc = gleam_types.loc_from_span(span)
      case container {
        g.Variable(_span, module_name) ->
          case env.resolve(tenv, module_name <> "/" <> label) {
            Ok(scheme_) -> instantiate(scheme_, loc)
            Error(_) -> {
              use target_type <- state.bind(infer_expr(tenv, container))
              use applied_target <- state.bind(type_apply_state(target_type))
              case applied_target {
                types.Tvar(_, _) ->
                  runtime.fatal(
                    "Record field access requires known type "
                    <> int.to_string(loc),
                  )
                _ -> {
                  let #(tname, targs) =
                    types.tcon_and_args(applied_target, [], loc)
                  let env.TEnv(_values, tcons, types_map, _aliases) = tenv
                  let constructors = case dict.get(types_map, tname) {
                    Ok(#(_arity, names)) -> set.to_list(names)
                    Error(_) -> runtime.fatal("Unknown type name " <> tname)
                  }
                  case constructors {
                    [cname] -> {
                      let #(free, cfields, _cres) = case
                        dict.get(tcons, cname)
                      {
                        Ok(value) -> value
                        Error(_) ->
                          runtime.fatal("Unknown constructor " <> cname)
                      }
                      let subst = dict.from_list(list.zip(free, targs))
                      let field_type =
                        types.type_apply(
                          subst,
                          lookup_constructor_field(label, cfields, loc),
                        )
                      state.pure(field_type)
                    }
                    _ ->
                      runtime.fatal(
                        "Record field access requires single-constructor type "
                        <> tname,
                      )
                  }
                }
              }
            }
          }
        _ -> {
          use target_type <- state.bind(infer_expr(tenv, container))
          use applied_target <- state.bind(type_apply_state(target_type))
          case applied_target {
            types.Tvar(_, _) ->
              runtime.fatal(
                "Record field access requires known type " <> int.to_string(loc),
              )
            _ -> {
              let #(tname, targs) = types.tcon_and_args(applied_target, [], loc)
              let env.TEnv(_values, tcons, types_map, _aliases) = tenv
              let constructors = case dict.get(types_map, tname) {
                Ok(#(_arity, names)) -> set.to_list(names)
                Error(_) -> runtime.fatal("Unknown type name " <> tname)
              }
              case constructors {
                [cname] -> {
                  let #(free, cfields, _cres) = case dict.get(tcons, cname) {
                    Ok(value) -> value
                    Error(_) -> runtime.fatal("Unknown constructor " <> cname)
                  }
                  let subst = dict.from_list(list.zip(free, targs))
                  let field_type =
                    types.type_apply(
                      subst,
                      lookup_constructor_field(label, cfields, loc),
                    )
                  state.pure(field_type)
                }
                _ ->
                  runtime.fatal(
                    "Record field access requires single-constructor type "
                    <> tname,
                  )
              }
            }
          }
        }
      }
    }

    g.Fn(span, params, return_annotation, body) -> {
      let loc = gleam_types.loc_from_span(span)
      use inferred <- state.bind(
        state.map_list(params, fn(param) {
          let g.FnParameter(name, annotation) = param
          case annotation {
            option.Some(type_expr) ->
              case gleam_types.type_(type_expr) {
                Ok(type_) -> state.pure(#(type_, bound_for_name(name, type_)))
                Error(_) ->
                  runtime.fatal(
                    "Unsupported type annotation " <> int.to_string(loc),
                  )
              }
            option.None -> {
              let param_name = case name {
                g.Named(name) -> name
                g.Discarded(_) -> "arg"
              }
              use arg_type <- state.bind(new_type_var(param_name, loc))
              state.pure(#(arg_type, bound_for_name(name, arg_type)))
            }
          }
        }),
      )
      let #(arg_types, scopes) = list.unzip(inferred)
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      use scope_applied <- state.bind(scope_apply_state(scope))
      let bound_env = env.with_scope(tenv, scope_applied)
      use body_type <- state.bind(infer_block(bound_env, body, loc))
      use _ignored <- state.bind(case return_annotation {
        option.Some(type_expr) ->
          case gleam_types.type_(type_expr) {
            Ok(type_) -> unify(body_type, type_, loc)
            Error(_) ->
              runtime.fatal(
                "Unsupported return annotation " <> int.to_string(loc),
              )
          }
        option.None -> state.pure(Nil)
      })
      use args_applied <- state.bind(
        state.map_list(arg_types, fn(arg) { type_apply_state(arg) }),
      )
      state.pure(types.Tfn(args_applied, body_type, loc))
    }

    g.Call(span, function, arguments) ->
      infer_call(tenv, span, function, arguments)

    g.FnCapture(span, label, function, args_before, args_after) ->
      infer_fn_capture(tenv, span, label, function, args_before, args_after)

    g.Case(span, subjects, clauses) -> infer_case(tenv, span, subjects, clauses)

    g.BinaryOperator(span, op, left, right) ->
      infer_binary_operator(tenv, span, op, left, right)
  }
}

fn infer_block(
  tenv: env.TEnv,
  statements: List(g.Statement),
  loc: Int,
) -> state.State(types.Type) {
  case statements {
    [] -> runtime.fatal("Empty block " <> int.to_string(loc))

    [g.Expression(expr)] -> infer_expr(tenv, expr)

    [g.Expression(_), ..] ->
      runtime.fatal("Block expression position " <> int.to_string(loc))

    [g.Assignment(_span, kind, pat, annotation, value), ..rest] ->
      case kind {
        g.Let -> infer_assignment(tenv, pat, annotation, value, rest, loc)
        g.LetAssert(_message) ->
          infer_assignment(tenv, pat, annotation, value, rest, loc)
      }

    [g.Assert(_span, expression, message), ..rest] -> {
      use expr_type <- state.bind(infer_expr(tenv, expression))
      use _ignored <- state.bind(unify(expr_type, types.Tcon("bool", loc), loc))
      use _ignored2 <- state.bind(case message {
        option.Some(msg) -> {
          use msg_type <- state.bind(infer_expr(tenv, msg))
          unify(msg_type, types.Tcon("string", loc), loc)
        }
        option.None -> state.pure(Nil)
      })
      infer_block(tenv, rest, loc)
    }

    [g.Use(span, patterns, function), ..rest] ->
      infer_use(tenv, span, patterns, function, rest, loc)
  }
}

fn infer_use(
  tenv: env.TEnv,
  _span: g.Span,
  patterns: List(g.UsePattern),
  function: g.Expression,
  rest: List(g.Statement),
  loc: Int,
) -> state.State(types.Type) {
  use args <- state.bind(infer_use_patterns(tenv, patterns, loc))
  let #(arg_types, scope) = args
  use result_var <- state.bind(new_type_var("use_result", loc))
  let callback_type = types.Tfn(arg_types, result_var, loc)
  use _ignored <- state.bind(infer_use_function(
    tenv,
    function,
    callback_type,
    result_var,
    loc,
  ))
  use scope_applied <- state.bind(scope_apply_state(scope))
  let bound_env = env.with_scope(tenv, scope_applied)
  use rest_type <- state.bind(infer_block(bound_env, rest, loc))
  use _ignored2 <- state.bind(unify(result_var, rest_type, loc))
  type_apply_state(rest_type)
}

fn infer_use_function(
  tenv: env.TEnv,
  function: g.Expression,
  callback_type: types.Type,
  result_var: types.Type,
  loc: Int,
) -> state.State(Nil) {
  case function {
    g.Call(_call_span, target, arguments) -> {
      let args = list.map(arguments, fn(field) { field_expr(field) })
      let exprs =
        list.map(args, fn(arg) {
          let #(_label, expr) = arg
          expr
        })
      use target_type <- state.bind(infer_expr_inner(tenv, target))
      use arg_types <- state.bind(
        state.map_list(exprs, fn(expr) {
          use arg_tenv <- state.bind(tenv_apply_state(tenv))
          infer_expr_inner(arg_tenv, expr)
        }),
      )
      use target_type_applied <- state.bind(type_apply_state(target_type))
      unify(
        target_type_applied,
        types.Tfn(list.append(arg_types, [callback_type]), result_var, loc),
        loc,
      )
    }
    _ -> {
      use target_type <- state.bind(infer_expr_inner(tenv, function))
      use target_type_applied <- state.bind(type_apply_state(target_type))
      unify(
        target_type_applied,
        types.Tfn([callback_type], result_var, loc),
        loc,
      )
    }
  }
}

fn infer_use_patterns(
  tenv: env.TEnv,
  patterns: List(g.UsePattern),
  loc: Int,
) -> state.State(#(List(types.Type), dict.Dict(String, scheme.Scheme))) {
  use inferred <- state.bind(
    state.map_list(patterns, fn(pattern) {
      let g.UsePattern(pat, annotation) = pattern
      use tuple <- state.bind(infer_pattern(tenv, pat))
      let #(pat_type, scope) = tuple
      use _ignored <- state.bind(case annotation {
        option.Some(type_expr) ->
          case gleam_types.type_(type_expr) {
            Ok(type_) -> unify(type_, pat_type, loc)
            Error(_) ->
              runtime.fatal("Unsupported annotation " <> int.to_string(loc))
          }
        option.None -> state.pure(Nil)
      })
      state.pure(#(pat_type, scope))
    }),
  )
  let #(types_, scopes) = list.unzip(inferred)
  let scope =
    list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
  state.pure(#(types_, scope))
}

fn infer_assignment(
  tenv: env.TEnv,
  pat: g.Pattern,
  annotation: option.Option(g.Type),
  value: g.Expression,
  rest: List(g.Statement),
  loc: Int,
) -> state.State(types.Type) {
  use value_type <- state.bind(infer_expr(tenv, value))
  use _ignored2 <- state.bind(case annotation {
    option.Some(type_expr) ->
      case gleam_types.type_(type_expr) {
        Ok(type_) -> unify(type_, value_type, loc)
        Error(_) ->
          runtime.fatal("Unsupported annotation " <> int.to_string(loc))
      }
    option.None -> state.pure(Nil)
  })
  case pat {
    g.PatternVariable(_span, name) -> {
      use applied_env <- state.bind(tenv_apply_state(tenv))
      let scheme_ = env.generalize(applied_env, value_type)
      let bound_env = env.with_type(applied_env, name, scheme_)
      infer_block(bound_env, rest, loc)
    }
    _ -> {
      use tuple <- state.bind(infer_pattern(tenv, pat))
      let #(type_, scope) = tuple
      use _ignored <- state.bind(unify(type_, value_type, loc))
      use scope_applied <- state.bind(scope_apply_state(scope))
      let bound_env = env.with_scope(tenv, scope_applied)
      infer_block(bound_env, rest, loc)
    }
  }
}

fn infer_call(
  tenv: env.TEnv,
  span: g.Span,
  function: g.Expression,
  arguments: List(g.Field(g.Expression)),
) -> state.State(types.Type) {
  let loc = gleam_types.loc_from_span(span)
  let args = list.map(arguments, fn(field) { field_expr(field) })
  let has_labels =
    list.any(args, fn(arg) {
      let #(label, _expr) = arg
      label != option.None
    })
  case has_labels, constructor_name(tenv, function) {
    True, option.Some(_) -> infer_constructor_call(tenv, loc, function, args)
    _, _ -> {
      let exprs =
        list.map(args, fn(arg) {
          let #(_label, expr) = arg
          expr
        })
      use result_var <- state.bind(new_type_var("result", loc))
      use target_type <- state.bind(infer_expr_inner(tenv, function))
      use arg_types <- state.bind(
        state.map_list(exprs, fn(expr) {
          use arg_tenv <- state.bind(tenv_apply_state(tenv))
          infer_expr_inner(arg_tenv, expr)
        }),
      )
      use target_type_applied <- state.bind(type_apply_state(target_type))
      use _ignored <- state.bind(unify(
        target_type_applied,
        types.Tfn(arg_types, result_var, loc),
        loc,
      ))
      type_apply_state(result_var)
    }
  }
}

fn constructor_name(
  tenv: env.TEnv,
  function: g.Expression,
) -> option.Option(String) {
  let name = case function {
    g.Variable(_span, name) -> option.Some(name)
    g.FieldAccess(_span, g.Variable(_, _module), name) -> option.Some(name)
    _ -> option.None
  }
  case name {
    option.None -> option.None
    option.Some(name) -> {
      let env.TEnv(_values, tcons, _types, _aliases) = tenv
      case dict.get(tcons, name) {
        Ok(_) -> option.Some(name)
        Error(_) -> option.None
      }
    }
  }
}

fn infer_constructor_call(
  tenv: env.TEnv,
  loc: Int,
  function: g.Expression,
  fields: List(#(option.Option(String), g.Expression)),
) -> state.State(types.Type) {
  let name = case function {
    g.Variable(_span, name) -> Ok(name)
    g.FieldAccess(_span, g.Variable(_, _module), name) -> Ok(name)
    _ -> Error(Nil)
  }
  case name {
    Error(_) ->
      runtime.fatal(
        "Labelled arguments require constructor " <> int.to_string(loc),
      )
    Ok(name) -> {
      use args2 <- state.bind(instantiate_tcon(tenv, name, loc))
      let #(cfields, cres) = args2
      let #(pairs, remaining) =
        match_constructor_fields(fields, cfields, loc, False)
      case remaining {
        [] -> Nil
        _ -> runtime.fatal("Constructor field mismatch " <> int.to_string(loc))
      }
      use _ignored <- state.bind(
        state.each_list(pairs, fn(pair) {
          let #(field, cfield) = pair
          let #(_label, expr) = field
          let #(_clabel, ctype) = cfield
          use expr_type <- state.bind(infer_expr(tenv, expr))
          unify(expr_type, ctype, loc)
        }),
      )
      type_apply_state(cres)
    }
  }
}

fn infer_fn_capture(
  tenv: env.TEnv,
  span: g.Span,
  label: option.Option(String),
  function: g.Expression,
  args_before: List(g.Field(g.Expression)),
  args_after: List(g.Field(g.Expression)),
) -> state.State(types.Type) {
  let loc = gleam_types.loc_from_span(span)
  case label {
    option.Some(_) ->
      runtime.fatal("Labelled capture not supported " <> int.to_string(loc))
    option.None -> {
      let before = list.map(args_before, fn(field) { field_expr(field) })
      let after = list.map(args_after, fn(field) { field_expr(field) })
      case
        list.any(before, fn(arg) {
          let #(label, _expr) = arg
          label != option.None
        })
        || list.any(after, fn(arg) {
          let #(label, _expr) = arg
          label != option.None
        })
      {
        True ->
          runtime.fatal(
            "Labelled capture arguments not supported " <> int.to_string(loc),
          )
        False -> {
          use hole_type <- state.bind(new_type_var("capture", loc))
          use result_var <- state.bind(new_type_var("result", loc))
          let before_exprs =
            list.map(before, fn(arg) {
              let #(_label, expr) = arg
              expr
            })
          let after_exprs =
            list.map(after, fn(arg) {
              let #(_label, expr) = arg
              expr
            })
          use target_type <- state.bind(infer_expr_inner(tenv, function))
          use before_types <- state.bind(
            state.map_list(before_exprs, fn(expr) {
              use arg_tenv <- state.bind(tenv_apply_state(tenv))
              infer_expr_inner(arg_tenv, expr)
            }),
          )
          use after_types <- state.bind(
            state.map_list(after_exprs, fn(expr) {
              use arg_tenv <- state.bind(tenv_apply_state(tenv))
              infer_expr_inner(arg_tenv, expr)
            }),
          )
          use target_type_applied <- state.bind(type_apply_state(target_type))
          let all_args = list.append(before_types, [hole_type, ..after_types])
          use _ignored2 <- state.bind(unify(
            target_type_applied,
            types.Tfn(all_args, result_var, loc),
            loc,
          ))
          type_apply_state(types.Tfn([hole_type], result_var, loc))
        }
      }
    }
  }
}

fn infer_case(
  tenv: env.TEnv,
  span: g.Span,
  subjects: List(g.Expression),
  clauses: List(g.Clause),
) -> state.State(types.Type) {
  let loc = gleam_types.loc_from_span(span)
  case subjects {
    [] -> runtime.fatal("Case subject count " <> int.to_string(loc))
    _ -> {
      use subject_types <- state.bind(
        state.map_list(subjects, fn(subject) { infer_expr(tenv, subject) }),
      )
      let target_type = case subject_types {
        [subject] -> subject
        _ -> types.Ttuple(subject_types, loc)
      }
      let subject_count = list.length(subjects)
      let cases =
        list.flatten(
          list.map(clauses, fn(clause) {
            clause_to_cases(clause, subject_count, span)
          }),
        )
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

fn clause_to_cases(
  clause: g.Clause,
  subject_count: Int,
  span: g.Span,
) -> List(#(g.Pattern, option.Option(g.Expression), g.Expression)) {
  let g.Clause(patterns, guard, body) = clause
  list.map(patterns, fn(patterns) {
    let pat = patterns_to_pat(patterns, subject_count, span)
    #(pat, guard, body)
  })
}

fn patterns_to_pat(
  patterns: List(g.Pattern),
  subject_count: Int,
  span: g.Span,
) -> g.Pattern {
  case subject_count {
    1 ->
      case patterns {
        [single] -> single
        _ ->
          runtime.fatal(
            "Case patterns " <> int.to_string(gleam_types.loc_from_span(span)),
          )
      }
    _ ->
      case patterns {
        [] ->
          runtime.fatal(
            "Case patterns " <> int.to_string(gleam_types.loc_from_span(span)),
          )
        _ ->
          case list.length(patterns) == subject_count {
            True -> g.PatternTuple(span, patterns)
            False ->
              runtime.fatal(
                "Case patterns "
                <> int.to_string(gleam_types.loc_from_span(span)),
              )
          }
      }
  }
}

fn infer_binary_operator(
  tenv: env.TEnv,
  span: g.Span,
  op: g.BinaryOperator,
  left: g.Expression,
  right: g.Expression,
) -> state.State(types.Type) {
  let loc = gleam_types.loc_from_span(span)
  case op {
    g.And | g.Or -> {
      use left_type <- state.bind(infer_expr(tenv, left))
      use right_type <- state.bind(infer_expr(tenv, right))
      use _ignored <- state.bind(unify(left_type, types.Tcon("bool", loc), loc))
      use _ignored2 <- state.bind(unify(
        right_type,
        types.Tcon("bool", loc),
        loc,
      ))
      state.pure(types.Tcon("bool", loc))
    }

    g.Eq | g.NotEq -> {
      use left_type <- state.bind(infer_expr(tenv, left))
      use right_type <- state.bind(infer_expr(tenv, right))
      use _ignored <- state.bind(unify(left_type, right_type, loc))
      state.pure(types.Tcon("bool", loc))
    }

    g.LtInt | g.LtEqInt | g.GtInt | g.GtEqInt -> {
      use left_type <- state.bind(infer_expr(tenv, left))
      use right_type <- state.bind(infer_expr(tenv, right))
      use _ignored <- state.bind(unify(left_type, types.Tcon("int", loc), loc))
      use _ignored2 <- state.bind(unify(right_type, types.Tcon("int", loc), loc))
      state.pure(types.Tcon("bool", loc))
    }

    g.LtFloat | g.LtEqFloat | g.GtFloat | g.GtEqFloat -> {
      use left_type <- state.bind(infer_expr(tenv, left))
      use right_type <- state.bind(infer_expr(tenv, right))
      use _ignored <- state.bind(unify(left_type, types.Tcon("float", loc), loc))
      use _ignored2 <- state.bind(unify(
        right_type,
        types.Tcon("float", loc),
        loc,
      ))
      state.pure(types.Tcon("bool", loc))
    }

    g.AddInt | g.SubInt | g.MultInt | g.DivInt | g.RemainderInt -> {
      use left_type <- state.bind(infer_expr(tenv, left))
      use right_type <- state.bind(infer_expr(tenv, right))
      use _ignored <- state.bind(unify(left_type, types.Tcon("int", loc), loc))
      use _ignored2 <- state.bind(unify(right_type, types.Tcon("int", loc), loc))
      state.pure(types.Tcon("int", loc))
    }

    g.AddFloat | g.SubFloat | g.MultFloat | g.DivFloat -> {
      use left_type <- state.bind(infer_expr(tenv, left))
      use right_type <- state.bind(infer_expr(tenv, right))
      use _ignored <- state.bind(unify(left_type, types.Tcon("float", loc), loc))
      use _ignored2 <- state.bind(unify(
        right_type,
        types.Tcon("float", loc),
        loc,
      ))
      state.pure(types.Tcon("float", loc))
    }

    g.Concatenate -> {
      use left_type <- state.bind(infer_expr(tenv, left))
      use right_type <- state.bind(infer_expr(tenv, right))
      use _ignored <- state.bind(unify(
        left_type,
        types.Tcon("string", loc),
        loc,
      ))
      use _ignored2 <- state.bind(unify(
        right_type,
        types.Tcon("string", loc),
        loc,
      ))
      state.pure(types.Tcon("string", loc))
    }

    g.Pipe -> infer_expr(tenv, pipe_to_call(span, left, right))
  }
}

fn pipe_to_call(
  span: g.Span,
  left: g.Expression,
  right: g.Expression,
) -> g.Expression {
  case right {
    g.Call(call_span, function, arguments) ->
      g.Call(call_span, function, [g.UnlabelledField(left), ..arguments])
    _ -> g.Call(span, right, [g.UnlabelledField(left)])
  }
}

pub fn infer_pattern(
  tenv: env.TEnv,
  pattern: g.Pattern,
) -> state.State(#(types.Type, dict.Dict(String, scheme.Scheme))) {
  case pattern {
    g.PatternVariable(span, name) -> {
      use v <- state.bind(new_type_var(name, gleam_types.loc_from_span(span)))
      let scope = dict.from_list([#(name, scheme.Forall(set.new(), v))])
      state.pure(#(v, scope))
    }

    g.PatternDiscard(span, _name) -> {
      use v <- state.bind(new_type_var("any", gleam_types.loc_from_span(span)))
      state.pure(#(v, dict.new()))
    }

    g.PatternString(span, _value) ->
      state.pure(#(
        types.Tcon("string", gleam_types.loc_from_span(span)),
        dict.new(),
      ))

    g.PatternInt(span, value) ->
      case int.parse(value) {
        Ok(_) ->
          state.pure(#(
            types.Tcon("int", gleam_types.loc_from_span(span)),
            dict.new(),
          ))
        Error(_) -> runtime.fatal("Invalid int pattern")
      }

    g.PatternFloat(span, value) ->
      case float.parse(value) {
        Ok(_) ->
          state.pure(#(
            types.Tcon("float", gleam_types.loc_from_span(span)),
            dict.new(),
          ))
        Error(_) -> runtime.fatal("Invalid float pattern")
      }

    g.PatternTuple(span, items) -> {
      use inferred <- state.bind(
        state.map_list(items, fn(item) { infer_pattern(tenv, item) }),
      )
      let #(types_, scopes) = list.unzip(inferred)
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      state.pure(#(types.Ttuple(types_, gleam_types.loc_from_span(span)), scope))
    }

    g.PatternList(span, items, tail) -> {
      let loc = gleam_types.loc_from_span(span)
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
        option.None -> {
          use list_type_applied <- state.bind(type_apply_state(list_type))
          state.pure(#(list_type_applied, scope))
        }
        option.Some(tail_pat) -> {
          use tail_tuple <- state.bind(infer_pattern(tenv, tail_pat))
          let #(tail_type, tail_scope) = tail_tuple
          use _ignored <- state.bind(unify(tail_type, list_type, loc))
          let scope = dict.merge(scope, tail_scope)
          use list_type_applied <- state.bind(type_apply_state(list_type))
          state.pure(#(list_type_applied, scope))
        }
      }
    }

    g.PatternAssignment(_span, pat, name) -> {
      use tuple <- state.bind(infer_pattern(tenv, pat))
      let #(type_, scope) = tuple
      let scope = dict.insert(scope, name, scheme.Forall(set.new(), type_))
      state.pure(#(type_, scope))
    }

    g.PatternConcatenate(span, _prefix, prefix_name, rest_name) -> {
      let loc = gleam_types.loc_from_span(span)
      let scope =
        dict.merge(
          case prefix_name {
            option.None -> dict.new()
            option.Some(name) ->
              case name {
                g.Named(name) ->
                  dict.from_list([
                    #(name, scheme.Forall(set.new(), types.Tcon("string", loc))),
                  ])
                g.Discarded(_) -> dict.new()
              }
          },
          case rest_name {
            g.Named(name) ->
              dict.from_list([
                #(name, scheme.Forall(set.new(), types.Tcon("string", loc))),
              ])
            g.Discarded(_) -> dict.new()
          },
        )
      state.pure(#(types.Tcon("string", loc), scope))
    }

    g.PatternBitString(span, segments) -> {
      use scopes <- state.bind(
        state.map_list(segments, fn(segment) {
          infer_bitstring_pat_segment(tenv, segment)
        }),
      )
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      state.pure(#(
        types.Tcon("bitstring", gleam_types.loc_from_span(span)),
        scope,
      ))
    }

    g.PatternVariant(span, _module, name, arguments, with_spread) -> {
      let loc = gleam_types.loc_from_span(span)
      use args2 <- state.bind(instantiate_tcon(tenv, name, loc))
      let #(cfields, cres) = args2
      let fields = list.map(arguments, fn(field) { pattern_field(field) })
      let #(pairs, remaining) =
        match_constructor_fields(fields, cfields, loc, with_spread)
      case remaining, with_spread {
        [], _ -> Nil
        _, True -> Nil
        _, False ->
          runtime.fatal("Constructor field mismatch " <> int.to_string(loc))
      }
      use inferred <- state.bind(
        state.map_list(pairs, fn(pair) {
          let #(field, cfield) = pair
          let #(_label, pat) = field
          let #(_clabel, ctype) = cfield
          use tuple <- state.bind(infer_pattern(tenv, pat))
          let #(ptype, scope) = tuple
          use _ignored <- state.bind(unify(ptype, ctype, loc))
          state.pure(scope)
        }),
      )
      use cres_applied <- state.bind(type_apply_state(cres))
      let scope =
        list.fold(inferred, dict.new(), fn(acc, scope) {
          dict.merge(acc, scope)
        })
      state.pure(#(cres_applied, scope))
    }
  }
}

fn bound_for_name(
  name: g.AssignmentName,
  type_: types.Type,
) -> dict.Dict(String, scheme.Scheme) {
  case name {
    g.Named(name) -> dict.from_list([#(name, scheme.Forall(set.new(), type_))])
    g.Discarded(_) -> dict.new()
  }
}

fn field_expr(
  field: g.Field(g.Expression),
) -> #(option.Option(String), g.Expression) {
  case field {
    g.UnlabelledField(item) -> #(option.None, item)
    g.LabelledField(label, _loc, item) -> #(option.Some(label), item)
    g.ShorthandField(label, loc) -> #(
      option.Some(label),
      g.Variable(loc, label),
    )
  }
}

fn pattern_field(
  field: g.Field(g.Pattern),
) -> #(option.Option(String), g.Pattern) {
  case field {
    g.UnlabelledField(item) -> #(option.None, item)
    g.LabelledField(label, _loc, item) -> #(option.Some(label), item)
    g.ShorthandField(label, loc) -> #(
      option.Some(label),
      g.PatternVariable(loc, label),
    )
  }
}

fn match_constructor_fields(
  fields: List(#(option.Option(String), a)),
  cfields: List(#(option.Option(String), b)),
  loc: Int,
  allow_missing: Bool,
) -> #(
  List(#(#(option.Option(String), a), #(option.Option(String), b))),
  List(#(option.Option(String), b)),
) {
  case fields {
    [] ->
      case allow_missing {
        True -> #([], cfields)
        False -> #([], cfields)
      }
    [field, ..rest] -> {
      let #(label, _value) = field
      case label {
        option.None ->
          case cfields {
            [] ->
              runtime.fatal("Constructor field mismatch " <> int.to_string(loc))
            [cfield, ..ctail] -> {
              let #(pairs, remaining) =
                match_constructor_fields(rest, ctail, loc, allow_missing)
              #([#(field, cfield), ..pairs], remaining)
            }
          }
        option.Some(name) -> {
          let #(maybe, remaining) = find_field(name, cfields, [])
          case maybe {
            option.None ->
              runtime.fatal(
                "Unknown field " <> name <> " " <> int.to_string(loc),
              )
            option.Some(cfield) -> {
              let #(pairs, remaining2) =
                match_constructor_fields(rest, remaining, loc, allow_missing)
              #([#(field, cfield), ..pairs], remaining2)
            }
          }
        }
      }
    }
  }
}

fn find_field(
  name: String,
  cfields: List(#(option.Option(String), b)),
  acc: List(#(option.Option(String), b)),
) -> #(
  option.Option(#(option.Option(String), b)),
  List(#(option.Option(String), b)),
) {
  case cfields {
    [] -> #(option.None, list.reverse(acc))
    [field, ..rest] -> {
      let #(label, _type) = field
      case label {
        option.Some(label_name) ->
          case label_name == name {
            True -> #(option.Some(field), list.append(list.reverse(acc), rest))
            False -> find_field(name, rest, [field, ..acc])
          }
        option.None -> find_field(name, rest, [field, ..acc])
      }
    }
  }
}

fn infer_bitstring_expr_options(
  tenv: env.TEnv,
  segment: #(g.Expression, List(g.BitStringSegmentOption(g.Expression))),
) -> state.State(Nil) {
  let #(_expr, options) = segment
  state.each_list(options, fn(opt) {
    case opt {
      g.SizeValueOption(expr) -> {
        use _ignored <- state.bind(infer_expr(tenv, expr))
        state.pure(Nil)
      }
      _ -> state.pure(Nil)
    }
  })
}

fn infer_bitstring_pat_segment(
  tenv: env.TEnv,
  segment: #(g.Pattern, List(g.BitStringSegmentOption(g.Pattern))),
) -> state.State(dict.Dict(String, scheme.Scheme)) {
  let #(pat, options) = segment
  use tuple <- state.bind(infer_pattern(tenv, pat))
  let #(_type, scope) = tuple
  use option_scope <- state.bind(infer_bitstring_pat_options(tenv, options))
  state.pure(dict.merge(scope, option_scope))
}

fn infer_bitstring_pat_options(
  tenv: env.TEnv,
  options: List(g.BitStringSegmentOption(g.Pattern)),
) -> state.State(dict.Dict(String, scheme.Scheme)) {
  state.foldl_list(options, dict.new(), fn(acc, opt) {
    case opt {
      g.SizeValueOption(pat) -> {
        use tuple <- state.bind(infer_pattern(tenv, pat))
        let #(_type, scope) = tuple
        state.pure(dict.merge(acc, scope))
      }
      _ -> state.pure(acc)
    }
  })
}

pub fn instantiate_tcon(
  tenv: env.TEnv,
  name: String,
  loc: Int,
) -> state.State(#(List(#(option.Option(String), types.Type)), types.Type)) {
  let env.TEnv(_values, tcons, _types, _aliases) = tenv
  case dict.get(tcons, name) {
    Error(_) -> runtime.fatal("Unknown type constructor: " <> name)
    Ok(#(free, cargs, cres)) -> {
      use subst <- state.bind(make_subst_for_free(set.from_list(free), loc))
      let args =
        list.map(cargs, fn(field) {
          let #(label, t) = field
          #(label, types.type_apply(subst, t))
        })
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
  use subst <- state.bind(state.get_subst())
  let left = types.type_apply(subst, t1)
  let right = types.type_apply(subst, t2)
  case left, right {
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
    types.Tfn(args1, res1, _), types.Tfn(args2, res2, _) ->
      case list.length(args1) == list.length(args2) {
        True -> {
          use _ignored <- state.bind(
            state.each_list(list.zip(args1, args2), fn(args) {
              let #(left, right) = args
              unify(left, right, loc)
            }),
          )
          use subst <- state.bind(state.get_subst())
          let left = types.type_apply(subst, res1)
          let right = types.type_apply(subst, res2)
          unify(left, right, loc)
        }
        False ->
          runtime.fatal(
            "Incompatible function arity "
            <> int.to_string(loc)
            <> " "
            <> runtime.jsonify(t1)
            <> " (vs) "
            <> runtime.jsonify(t2),
          )
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

fn lookup_constructor_field(
  label: String,
  cfields: List(#(option.Option(String), types.Type)),
  loc: Int,
) -> types.Type {
  case cfields {
    [] -> runtime.fatal("Unknown field " <> label <> " " <> int.to_string(loc))
    [field, ..rest] -> {
      let #(maybe, t) = field
      case maybe {
        option.Some(name) ->
          case name == label {
            True -> t
            False -> lookup_constructor_field(label, rest, loc)
          }
        option.None -> lookup_constructor_field(label, rest, loc)
      }
    }
  }
}
