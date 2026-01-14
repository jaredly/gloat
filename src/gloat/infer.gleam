import glance as g
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/set
import gloat/env
import gloat/exhaustive
import gloat/gleam_types
import gloat/infer_state as is
import gloat/scheme
import gloat/state
import gloat/types

pub fn infer_expr(
  tenv: env.TEnv,
  expr: g.Expression,
) -> is.InferState(types.Type) {
  use old_subst <- is.bind(reset_subst_state(dict.new()))
  use type_ <- is.bind(infer_expr_inner(tenv, expr))
  use new_subst <- is.bind(reset_subst_state(old_subst))
  use _ignored <- is.bind(put_subst_state(new_subst))
  is.ok(type_)
}

fn infer_expr_inner(
  tenv: env.TEnv,
  expr: g.Expression,
) -> is.InferState(types.Type) {
  case expr {
    g.Int(span, value) ->
      case int.parse(value) {
        Ok(_) -> is.ok(types.Tcon("Int", span))
        Error(_) -> is.error("Invalid int literal", span)
      }

    g.Float(span, value) ->
      case float.parse(value) {
        Ok(_) -> is.ok(types.Tcon("Float", span))
        Error(_) -> is.error("Invalid float literal", span)
      }

    g.String(span, _value) -> is.ok(types.Tcon("String", span))

    g.Variable(span, name) ->
      case env.resolve(tenv, name) {
        Ok(scheme_) -> instantiate(scheme_, span)
        Error(_) -> is.error("Variable not found in scope: " <> name, span)
      }

    g.NegateInt(span, value) -> {
      let _loc = gleam_types.loc_from_span(span)
      use value_type <- is.bind(infer_expr(tenv, value))
      use _ignored <- is.bind(unify(value_type, types.Tcon("Int", span), span))
      is.ok(types.Tcon("Int", span))
    }

    g.NegateBool(span, value) -> {
      let _loc = gleam_types.loc_from_span(span)
      use value_type <- is.bind(infer_expr(tenv, value))
      use _ignored <- is.bind(unify(value_type, types.Tcon("Bool", span), span))
      is.ok(types.Tcon("Bool", span))
    }

    g.Block(span, statements) -> infer_block(tenv, statements, span)

    g.Panic(span, message) -> {
      let _loc = gleam_types.loc_from_span(span)
      use _ignored <- is.bind(case message {
        option.Some(expr) -> {
          use msg_type <- is.bind(infer_expr(tenv, expr))
          unify(msg_type, types.Tcon("String", span), span)
        }
        option.None -> is.ok(Nil)
      })
      new_type_var("panic", span)
    }

    g.Todo(span, message) -> {
      let _loc = gleam_types.loc_from_span(span)
      use _ignored <- is.bind(case message {
        option.Some(expr) -> {
          use msg_type <- is.bind(infer_expr(tenv, expr))
          unify(msg_type, types.Tcon("String", span), span)
        }
        option.None -> is.ok(Nil)
      })
      new_type_var("todo", span)
    }

    g.Tuple(span, items) -> {
      use item_types <- is.bind(
        is.map_list(items, fn(item) { infer_expr(tenv, item) }),
      )
      is.ok(types.Ttuple(item_types, span))
    }

    g.TupleIndex(span, target, index) -> {
      use target_type <- is.bind(infer_expr(tenv, target))
      use applied_target <- is.bind(type_apply_state(target_type))
      case applied_target {
        types.Ttuple(args, _) -> tuple_index_type(args, index, span)
        types.Tvar(_, _) -> {
          use args <- is.bind(tuple_index_vars(index + 1, span))
          let tuple_type = types.Ttuple(args, span)
          use _ignored <- is.bind(unify(applied_target, tuple_type, span))
          tuple_index_type(args, index, span)
        }
        _ -> is.error("Tuple index on non-tuple", span)
      }
    }

    g.List(span, items, tail) -> {
      let _loc = gleam_types.loc_from_span(span)
      use elem_type <- is.bind(new_type_var("list_item", span))
      let list_type = types.Tapp(types.Tcon("List", span), [elem_type], span)
      use item_types <- is.bind(
        is.map_list(items, fn(item) { infer_expr(tenv, item) }),
      )
      use _ignored <- is.bind(
        is.each_list(item_types, fn(item_type) {
          unify(item_type, elem_type, span)
        }),
      )
      case tail {
        option.None -> type_apply_state(list_type)
        option.Some(tail_expr) -> {
          use tail_type <- is.bind(infer_expr(tenv, tail_expr))
          use _ignored <- is.bind(unify(tail_type, list_type, span))
          type_apply_state(list_type)
        }
      }
    }

    g.BitString(span, segments) -> {
      let _loc = gleam_types.loc_from_span(span)
      use _ignored <- is.bind(
        is.each_list(segments, fn(segment) {
          let #(expr, _opts) = segment
          use _ignored2 <- is.bind(infer_expr(tenv, expr))
          use _ignored3 <- is.bind(infer_bitstring_expr_options(tenv, segment))
          is.ok(Nil)
        }),
      )
      is.ok(types.Tcon("BitString", span))
    }

    g.Echo(span, value, message) -> {
      let _loc = gleam_types.loc_from_span(span)
      use _ignored <- is.bind(case message {
        option.Some(expr) -> {
          use msg_type <- is.bind(infer_expr(tenv, expr))
          use _ignored2 <- is.bind(unify(
            msg_type,
            types.Tcon("String", span),
            span,
          ))
          is.ok(Nil)
        }
        option.None -> is.ok(Nil)
      })
      case value {
        option.Some(expr) -> infer_expr(tenv, expr)
        option.None -> is.ok(types.Tcon("()", span))
      }
    }

    g.RecordUpdate(span, _module, name, record, fields) -> {
      let _loc = gleam_types.loc_from_span(span)
      use args2 <- is.bind(instantiate_tcon(tenv, name, span))
      let #(cfields, cres) = args2
      use record_type <- is.bind(infer_expr(tenv, record))
      use _ignored <- is.bind(unify(record_type, cres, span))
      use _ignored2 <- is.bind(
        is.each_list(fields, fn(field) {
          let g.RecordUpdateField(label, item) = field
          use ctype <- is.bind(lookup_constructor_field(label, cfields, span))
          let expr = case item {
            option.Some(expr) -> expr
            option.None -> g.Variable(span, label)
          }
          use expr_type <- is.bind(infer_expr(tenv, expr))
          unify(expr_type, ctype, span)
        }),
      )
      type_apply_state(cres)
    }

    g.FieldAccess(span, container, label) -> {
      case container {
        g.Variable(_span, module_name) ->
          case env.resolve_module(tenv, module_name) {
            Ok(module_key) ->
              case env.resolve(tenv, module_key <> "/" <> label) {
                Ok(scheme_) -> instantiate(scheme_, span)
                Error(_) ->
                  is.error(
                    "Unknown module value " <> module_key <> "/" <> label,
                    span,
                  )
              }
            Error(_) -> {
              use target_type <- is.bind(infer_expr(tenv, container))
              use applied_target <- is.bind(type_apply_state(target_type))
              case applied_target {
                types.Tvar(_, _) ->
                  is.error("Record field access requires known type", span)
                _ -> {
                  use tcon <- is.bind(
                    is.from_result(types.tcon_and_args(applied_target, [], span)),
                  )
                  let #(tname, targs) = tcon
                  let env.TEnv(_values, tcons, types_map, _aliases, _modules) =
                    tenv
                  case dict.get(types_map, tname) {
                    Error(_) -> is.error("Unknown type name " <> tname, span)
                    Ok(#(_arity, names)) -> {
                      let constructors = set.to_list(names)
                      case constructors {
                        [cname] -> {
                          case dict.get(tcons, cname) {
                            Ok(value) -> {
                              let #(free, cfields, _cres) = value
                              let subst = dict.from_list(list.zip(free, targs))
                              use ctype <- is.bind(lookup_constructor_field(
                                label,
                                cfields,
                                span,
                              ))
                              let field_type = types.type_apply(subst, ctype)
                              is.ok(field_type)
                            }
                            Error(_) ->
                              is.error("Unknown constructor " <> cname, span)
                          }
                        }
                        _ ->
                          is.error(
                            "Record field access requires single-constructor type "
                              <> tname,
                            span,
                          )
                      }
                    }
                  }
                }
              }
            }
          }
        _ -> {
          use target_type <- is.bind(infer_expr(tenv, container))
          use applied_target <- is.bind(type_apply_state(target_type))
          case applied_target {
            types.Tvar(_, _) ->
              is.error("Record field access requires known type", span)
            _ -> {
              use tcon <- is.bind(
                is.from_result(types.tcon_and_args(applied_target, [], span)),
              )
              let #(tname, targs) = tcon
              let env.TEnv(_values, tcons, types_map, _aliases, _modules) = tenv
              case dict.get(types_map, tname) {
                Error(_) -> is.error("Unknown type name " <> tname, span)
                Ok(#(_arity, names)) -> {
                  let constructors = set.to_list(names)
                  case constructors {
                    [cname] -> {
                      case dict.get(tcons, cname) {
                        Ok(value) -> {
                          let #(free, cfields, _cres) = value
                          let subst = dict.from_list(list.zip(free, targs))
                          use ctype <- is.bind(lookup_constructor_field(
                            label,
                            cfields,
                            span,
                          ))
                          let field_type = types.type_apply(subst, ctype)
                          is.ok(field_type)
                        }
                        Error(_) ->
                          is.error("Unknown constructor " <> cname, span)
                      }
                    }
                    _ ->
                      is.error(
                        "Record field access requires single-constructor type "
                          <> tname,
                        span,
                      )
                  }
                }
              }
            }
          }
        }
      }
    }

    g.Fn(span, params, return_annotation, body) -> {
      use inferred <- is.bind(
        is.map_list(params, fn(param) {
          let g.FnParameter(name, annotation) = param
          case annotation {
            option.Some(type_expr) ->
              case gleam_types.type_(type_expr) {
                Ok(type_) -> is.ok(#(type_, bound_for_name(name, type_)))
                Error(_) -> is.error("Unsupported type annotation", span)
              }
            option.None -> {
              let param_name = case name {
                g.Named(name) -> name
                g.Discarded(_) -> "arg"
              }
              use arg_type <- is.bind(new_type_var(param_name, span))
              is.ok(#(arg_type, bound_for_name(name, arg_type)))
            }
          }
        }),
      )
      let #(arg_types, scopes) = list.unzip(inferred)
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      use scope_applied <- is.bind(scope_apply_state(scope))
      let bound_env = env.with_scope(tenv, scope_applied)
      use body_type <- is.bind(infer_block(bound_env, body, span))
      use _ignored <- is.bind(case return_annotation {
        option.Some(type_expr) ->
          case gleam_types.type_(type_expr) {
            Ok(type_) -> unify(body_type, type_, span)
            Error(_) -> is.error("Unsupported return annotation", span)
          }
        option.None -> is.ok(Nil)
      })
      use args_applied <- is.bind(
        is.map_list(arg_types, fn(arg) { type_apply_state(arg) }),
      )
      is.ok(types.Tfn(args_applied, body_type, span))
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
  span: g.Span,
) -> is.InferState(types.Type) {
  case statements {
    [] -> is.error("Empty block", span)

    [g.Expression(expr)] -> infer_expr(tenv, expr)

    [g.Expression(expr), ..rest] -> {
      use _ignored <- is.bind(infer_expr(tenv, expr))
      infer_block(tenv, rest, span)
    }

    [g.Assignment(assign_span, kind, pat, annotation, value), ..rest] ->
      case kind {
        g.Let ->
          infer_assignment(tenv, pat, annotation, value, rest, assign_span)
        g.LetAssert(_message) ->
          infer_assignment(tenv, pat, annotation, value, rest, assign_span)
      }

    [g.Assert(span, expression, message), ..rest] -> {
      use expr_type <- is.bind(infer_expr(tenv, expression))
      use _ignored <- is.bind(unify(expr_type, types.Tcon("Bool", span), span))
      use _ignored2 <- is.bind(case message {
        option.Some(msg) -> {
          use msg_type <- is.bind(infer_expr(tenv, msg))
          unify(msg_type, types.Tcon("String", span), span)
        }
        option.None -> is.ok(Nil)
      })
      infer_block(tenv, rest, span)
    }

    [g.Use(span, patterns, function), ..rest] ->
      infer_use(tenv, span, patterns, function, rest, span)
  }
}

fn infer_use(
  tenv: env.TEnv,
  span: g.Span,
  patterns: List(g.UsePattern),
  function: g.Expression,
  rest: List(g.Statement),
  block_span: g.Span,
) -> is.InferState(types.Type) {
  use args <- is.bind(infer_use_patterns(tenv, patterns, span))
  let #(arg_types, scope) = args
  use result_var <- is.bind(new_type_var("use_result", span))
  let callback_type = types.Tfn(arg_types, result_var, span)
  use _ignored <- is.bind(infer_use_function(
    tenv,
    span,
    function,
    callback_type,
    result_var,
    span,
  ))
  use scope_applied <- is.bind(scope_apply_state(scope))
  let bound_env = env.with_scope(tenv, scope_applied)
  use rest_type <- is.bind(infer_block(bound_env, rest, block_span))
  use _ignored2 <- is.bind(unify(result_var, rest_type, block_span))
  type_apply_state(rest_type)
}

fn infer_use_function(
  tenv: env.TEnv,
  span: g.Span,
  function: g.Expression,
  callback_type: types.Type,
  result_var: types.Type,
  call_span: g.Span,
) -> is.InferState(Nil) {
  case function {
    g.Call(_call_span, target, arguments) -> {
      let args = list.map(arguments, fn(field) { field_expr(field) })
      let exprs =
        list.map(args, fn(arg) {
          let #(_label, expr) = arg
          expr
        })
      use target_type <- is.bind(infer_expr_inner(tenv, target))
      use arg_types <- is.bind(
        is.map_list(exprs, fn(expr) {
          use arg_tenv <- is.bind(tenv_apply_state(tenv))
          infer_expr_inner(arg_tenv, expr)
        }),
      )
      use target_type_applied <- is.bind(type_apply_state(target_type))
      unify(
        target_type_applied,
        types.Tfn(list.append(arg_types, [callback_type]), result_var, span),
        call_span,
      )
    }
    _ -> {
      use target_type <- is.bind(infer_expr_inner(tenv, function))
      use target_type_applied <- is.bind(type_apply_state(target_type))
      unify(
        target_type_applied,
        types.Tfn([callback_type], result_var, span),
        call_span,
      )
    }
  }
}

fn infer_use_patterns(
  tenv: env.TEnv,
  patterns: List(g.UsePattern),
  span: g.Span,
) -> is.InferState(#(List(types.Type), dict.Dict(String, scheme.Scheme))) {
  use inferred <- is.bind(
    is.map_list(patterns, fn(pattern) {
      let g.UsePattern(pat, annotation) = pattern
      use tuple <- is.bind(infer_pattern(tenv, pat))
      let #(pat_type, scope) = tuple
      use _ignored <- is.bind(case annotation {
        option.Some(type_expr) ->
          case gleam_types.type_(type_expr) {
            Ok(type_) -> unify(type_, pat_type, span)
            Error(_) -> is.error("Unsupported annotation", span)
          }
        option.None -> is.ok(Nil)
      })
      is.ok(#(pat_type, scope))
    }),
  )
  let #(types_, scopes) = list.unzip(inferred)
  let scope =
    list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
  is.ok(#(types_, scope))
}

fn infer_assignment(
  tenv: env.TEnv,
  pat: g.Pattern,
  annotation: option.Option(g.Type),
  value: g.Expression,
  rest: List(g.Statement),
  span: g.Span,
) -> is.InferState(types.Type) {
  use value_type <- is.bind(infer_expr(tenv, value))
  use _ignored2 <- is.bind(case annotation {
    option.Some(type_expr) ->
      case gleam_types.type_(type_expr) {
        Ok(type_) -> unify(type_, value_type, span)
        Error(_) -> is.error("Unsupported annotation", span)
      }
    option.None -> is.ok(Nil)
  })
  case pat {
    g.PatternVariable(_span, name) -> {
      use applied_env <- is.bind(tenv_apply_state(tenv))
      let scheme_ = env.generalize(applied_env, value_type)
      let bound_env = env.with_type(applied_env, name, scheme_)
      infer_block(bound_env, rest, span)
    }
    _ -> {
      use tuple <- is.bind(infer_pattern(tenv, pat))
      let #(type_, scope) = tuple
      use _ignored <- is.bind(unify(type_, value_type, span))
      use scope_applied <- is.bind(scope_apply_state(scope))
      let bound_env = env.with_scope(tenv, scope_applied)
      infer_block(bound_env, rest, span)
    }
  }
}

fn infer_call(
  tenv: env.TEnv,
  span: g.Span,
  function: g.Expression,
  arguments: List(g.Field(g.Expression)),
) -> is.InferState(types.Type) {
  let args = list.map(arguments, fn(field) { field_expr(field) })
  let has_labels =
    list.any(args, fn(arg) {
      let #(label, _expr) = arg
      label != option.None
    })
  case has_labels, constructor_name(tenv, function) {
    True, option.Some(_) -> infer_constructor_call(tenv, span, function, args)
    _, _ -> {
      let exprs =
        list.map(args, fn(arg) {
          let #(_label, expr) = arg
          expr
        })
      use result_var <- is.bind(new_type_var("result", span))
      use target_type <- is.bind(infer_expr_inner(tenv, function))
      use arg_types <- is.bind(
        is.map_list(exprs, fn(expr) {
          use arg_tenv <- is.bind(tenv_apply_state(tenv))
          infer_expr_inner(arg_tenv, expr)
        }),
      )
      use target_type_applied <- is.bind(type_apply_state(target_type))
      use _ignored <- is.bind(unify(
        target_type_applied,
        types.Tfn(arg_types, result_var, span),
        span,
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
      let env.TEnv(_values, tcons, _types, _aliases, _modules) = tenv
      case dict.get(tcons, name) {
        Ok(_) -> option.Some(name)
        Error(_) -> option.None
      }
    }
  }
}

fn infer_constructor_call(
  tenv: env.TEnv,
  span: g.Span,
  function: g.Expression,
  fields: List(#(option.Option(String), g.Expression)),
) -> is.InferState(types.Type) {
  let name = case function {
    g.Variable(_span, name) -> Ok(name)
    g.FieldAccess(_span, g.Variable(_, _module), name) -> Ok(name)
    _ -> Error(Nil)
  }
  case name {
    Error(_) -> is.error("Labelled arguments require constructor", span)
    Ok(name) -> {
      use args2 <- is.bind(instantiate_tcon(tenv, name, span))
      let #(cfields, cres) = args2
      use matched <- is.bind(match_constructor_fields(
        fields,
        cfields,
        span,
        False,
      ))
      let #(pairs, remaining) = matched
      use _ignored_remaining <- is.bind(case remaining {
        [] -> is.ok(Nil)
        _ -> is.error("Constructor field mismatch", span)
      })
      use _ignored <- is.bind(
        is.each_list(pairs, fn(pair) {
          let #(field, cfield) = pair
          let #(_label, expr) = field
          let #(_clabel, ctype) = cfield
          use expr_type <- is.bind(infer_expr(tenv, expr))
          unify(expr_type, ctype, span)
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
) -> is.InferState(types.Type) {
  case label {
    option.Some(_) -> is.error("Labelled capture not supported", span)
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
        True -> is.error("Labelled capture arguments not supported", span)
        False -> {
          use hole_type <- is.bind(new_type_var("capture", span))
          use result_var <- is.bind(new_type_var("result", span))
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
          use target_type <- is.bind(infer_expr_inner(tenv, function))
          use before_types <- is.bind(
            is.map_list(before_exprs, fn(expr) {
              use arg_tenv <- is.bind(tenv_apply_state(tenv))
              infer_expr_inner(arg_tenv, expr)
            }),
          )
          use after_types <- is.bind(
            is.map_list(after_exprs, fn(expr) {
              use arg_tenv <- is.bind(tenv_apply_state(tenv))
              infer_expr_inner(arg_tenv, expr)
            }),
          )
          use target_type_applied <- is.bind(type_apply_state(target_type))
          let all_args = list.append(before_types, [hole_type, ..after_types])
          use _ignored2 <- is.bind(unify(
            target_type_applied,
            types.Tfn(all_args, result_var, span),
            span,
          ))
          type_apply_state(types.Tfn([hole_type], result_var, span))
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
) -> is.InferState(types.Type) {
  case subjects {
    [] -> is.error("Case subject count", span)
    _ -> {
      use subject_types <- is.bind(
        is.map_list(subjects, fn(subject) { infer_expr(tenv, subject) }),
      )
      let target_type = case subject_types {
        [subject] -> subject
        _ -> types.Ttuple(subject_types, span)
      }
      let subject_count = list.length(subjects)
      use cases_groups <- is.bind(
        is.map_list(clauses, fn(clause) {
          clause_to_cases(clause, subject_count, span)
        }),
      )
      let cases = list.flatten(cases_groups)
      use result_type <- is.bind(new_type_var("match result", span))
      use result_pair <- is.bind(
        is.foldl_list(cases, #(target_type, result_type), fn(args, args2) {
          let #(target_type_inner, result) = args
          let #(pat, guard, body) = args2
          use args_inner <- is.bind(infer_pattern(tenv, pat))
          let #(type_, scope) = args_inner
          use _ignored <- is.bind(unify(type_, target_type_inner, span))
          use scope_applied <- is.bind(scope_apply_state(scope))
          let bound_env = env.with_scope(tenv, scope_applied)
          use _ignored_guard <- is.bind(case guard {
            option.Some(expr) -> {
              use guard_type <- is.bind(infer_expr(bound_env, expr))
              unify(guard_type, types.Tcon("Bool", span), span)
            }
            option.None -> is.ok(Nil)
          })
          use body_type <- is.bind(infer_expr(bound_env, body))
          use subst <- is.bind(get_subst_state())
          use _ignored2 <- is.bind(unify(
            types.type_apply(subst, result),
            body_type,
            span,
          ))
          use subst2 <- is.bind(get_subst_state())
          let next_target = types.type_apply(subst2, target_type_inner)
          let next_result = types.type_apply(subst2, result)
          is.ok(#(next_target, next_result))
        }),
      )
      let #(_target, final_result) = result_pair
      use target_applied <- is.bind(type_apply_state(target_type))
      case
        list.any(cases, fn(args) {
          let #(_pat, guard, _body) = args
          case guard {
            option.None -> False
            option.Some(_) -> True
          }
        })
      {
        True -> is.ok(Nil)
        False ->
          is.bind(
            exhaustive.check_exhaustiveness(
              tenv,
              target_applied,
              list.map(cases, fn(args) {
                let #(pat, _guard, _body) = args
                pat
              }),
              span,
            ),
            is.ok,
          )
      }
      is.ok(final_result)
    }
  }
}

fn clause_to_cases(
  clause: g.Clause,
  subject_count: Int,
  span: g.Span,
) -> is.InferState(
  List(#(g.Pattern, option.Option(g.Expression), g.Expression)),
) {
  let g.Clause(patterns, guard, body) = clause
  use pats <- is.bind(
    is.map_list(patterns, fn(patterns) {
      use pat <- is.bind(patterns_to_pat(patterns, subject_count, span))
      is.ok(#(pat, guard, body))
    }),
  )
  is.ok(pats)
}

fn patterns_to_pat(
  patterns: List(g.Pattern),
  subject_count: Int,
  span: g.Span,
) -> is.InferState(g.Pattern) {
  case subject_count {
    1 ->
      case patterns {
        [single] -> is.ok(single)
        _ -> is.error("Case patterns", span)
      }
    _ ->
      case patterns {
        [] -> is.error("Case patterns", span)
        _ ->
          case list.length(patterns) == subject_count {
            True -> is.ok(g.PatternTuple(span, patterns))
            False -> is.error("Case patterns", span)
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
) -> is.InferState(types.Type) {
  case op {
    g.And | g.Or -> {
      use left_type <- is.bind(infer_expr(tenv, left))
      use right_type <- is.bind(infer_expr(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("Bool", span), span))
      use _ignored2 <- is.bind(unify(right_type, types.Tcon("Bool", span), span))
      is.ok(types.Tcon("Bool", span))
    }

    g.Eq | g.NotEq -> {
      use left_type <- is.bind(infer_expr(tenv, left))
      use right_type <- is.bind(infer_expr(tenv, right))
      use _ignored <- is.bind(unify(left_type, right_type, span))
      is.ok(types.Tcon("Bool", span))
    }

    g.LtInt | g.LtEqInt | g.GtInt | g.GtEqInt -> {
      use left_type <- is.bind(infer_expr(tenv, left))
      use right_type <- is.bind(infer_expr(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("Int", span), span))
      use _ignored2 <- is.bind(unify(right_type, types.Tcon("Int", span), span))
      is.ok(types.Tcon("Bool", span))
    }

    g.LtFloat | g.LtEqFloat | g.GtFloat | g.GtEqFloat -> {
      use left_type <- is.bind(infer_expr(tenv, left))
      use right_type <- is.bind(infer_expr(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("Float", span), span))
      use _ignored2 <- is.bind(unify(
        right_type,
        types.Tcon("Float", span),
        span,
      ))
      is.ok(types.Tcon("Bool", span))
    }

    g.AddInt | g.SubInt | g.MultInt | g.DivInt | g.RemainderInt -> {
      use left_type <- is.bind(infer_expr(tenv, left))
      use right_type <- is.bind(infer_expr(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("Int", span), span))
      use _ignored2 <- is.bind(unify(right_type, types.Tcon("Int", span), span))
      is.ok(types.Tcon("Int", span))
    }

    g.AddFloat | g.SubFloat | g.MultFloat | g.DivFloat -> {
      use left_type <- is.bind(infer_expr(tenv, left))
      use right_type <- is.bind(infer_expr(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("Float", span), span))
      use _ignored2 <- is.bind(unify(
        right_type,
        types.Tcon("Float", span),
        span,
      ))
      is.ok(types.Tcon("Float", span))
    }

    g.Concatenate -> {
      use left_type <- is.bind(infer_expr(tenv, left))
      use right_type <- is.bind(infer_expr(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("String", span), span))
      use _ignored2 <- is.bind(unify(
        right_type,
        types.Tcon("String", span),
        span,
      ))
      is.ok(types.Tcon("String", span))
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
    g.Call(call_span, function, arguments) -> {
      let piped = case split_at_unlabelled(arguments, []) {
        Ok(#(prefix, rest)) ->
          list.append(prefix, [g.UnlabelledField(left), ..rest])
        Error(_) ->
          case arguments {
            [] -> [g.UnlabelledField(left)]
            [first, ..rest] -> [first, g.UnlabelledField(left), ..rest]
          }
      }
      g.Call(call_span, function, piped)
    }
    _ -> g.Call(span, right, [g.UnlabelledField(left)])
  }
}

fn split_at_unlabelled(
  arguments: List(g.Field(g.Expression)),
  acc: List(g.Field(g.Expression)),
) -> Result(#(List(g.Field(g.Expression)), List(g.Field(g.Expression))), Nil) {
  case arguments {
    [] -> Error(Nil)
    [first, ..rest] ->
      case first {
        g.UnlabelledField(_) -> Ok(#(list.reverse(acc), [first, ..rest]))
        _ -> split_at_unlabelled(rest, [first, ..acc])
      }
  }
}

pub fn infer_pattern(
  tenv: env.TEnv,
  pattern: g.Pattern,
) -> is.InferState(#(types.Type, dict.Dict(String, scheme.Scheme))) {
  case pattern {
    g.PatternVariable(span, name) -> {
      use v <- is.bind(new_type_var(name, span))
      let scope = dict.from_list([#(name, scheme.Forall(set.new(), v))])
      is.ok(#(v, scope))
    }

    g.PatternDiscard(span, _name) -> {
      use v <- is.bind(new_type_var("any", span))
      is.ok(#(v, dict.new()))
    }

    g.PatternString(span, _value) ->
      is.ok(#(types.Tcon("String", span), dict.new()))

    g.PatternInt(span, value) ->
      case int.parse(value) {
        Ok(_) -> is.ok(#(types.Tcon("Int", span), dict.new()))
        Error(_) -> is.error("Invalid int pattern", span)
      }

    g.PatternFloat(span, value) ->
      case float.parse(value) {
        Ok(_) -> is.ok(#(types.Tcon("Float", span), dict.new()))
        Error(_) -> is.error("Invalid float pattern", span)
      }

    g.PatternTuple(span, items) -> {
      use inferred <- is.bind(
        is.map_list(items, fn(item) { infer_pattern(tenv, item) }),
      )
      let #(types_, scopes) = list.unzip(inferred)
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      is.ok(#(types.Ttuple(types_, span), scope))
    }

    g.PatternList(span, items, tail) -> {
      let _loc = gleam_types.loc_from_span(span)
      use elem_type <- is.bind(new_type_var("list_item", span))
      let list_type = types.Tapp(types.Tcon("List", span), [elem_type], span)
      use inferred <- is.bind(
        is.map_list(items, fn(item) { infer_pattern(tenv, item) }),
      )
      let #(item_types, scopes) = list.unzip(inferred)
      use _ignored <- is.bind(
        is.each_list(item_types, fn(item_type) {
          unify(item_type, elem_type, span)
        }),
      )
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      case tail {
        option.None -> {
          use list_type_applied <- is.bind(type_apply_state(list_type))
          is.ok(#(list_type_applied, scope))
        }
        option.Some(tail_pat) -> {
          use tail_tuple <- is.bind(infer_pattern(tenv, tail_pat))
          let #(tail_type, tail_scope) = tail_tuple
          use _ignored <- is.bind(unify(tail_type, list_type, span))
          let scope = dict.merge(scope, tail_scope)
          use list_type_applied <- is.bind(type_apply_state(list_type))
          is.ok(#(list_type_applied, scope))
        }
      }
    }

    g.PatternAssignment(_span, pat, name) -> {
      use tuple <- is.bind(infer_pattern(tenv, pat))
      let #(type_, scope) = tuple
      let scope = dict.insert(scope, name, scheme.Forall(set.new(), type_))
      is.ok(#(type_, scope))
    }

    g.PatternConcatenate(span, _prefix, prefix_name, rest_name) -> {
      let _loc = gleam_types.loc_from_span(span)
      let scope =
        dict.merge(
          case prefix_name {
            option.None -> dict.new()
            option.Some(name) ->
              case name {
                g.Named(name) ->
                  dict.from_list([
                    #(
                      name,
                      scheme.Forall(set.new(), types.Tcon("String", span)),
                    ),
                  ])
                g.Discarded(_) -> dict.new()
              }
          },
          case rest_name {
            g.Named(name) ->
              dict.from_list([
                #(name, scheme.Forall(set.new(), types.Tcon("String", span))),
              ])
            g.Discarded(_) -> dict.new()
          },
        )
      is.ok(#(types.Tcon("String", span), scope))
    }

    g.PatternBitString(span, segments) -> {
      use scopes <- is.bind(
        is.map_list(segments, fn(segment) {
          infer_bitstring_pat_segment(tenv, segment)
        }),
      )
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      is.ok(#(types.Tcon("BitString", span), scope))
    }

    g.PatternVariant(span, _module, name, arguments, with_spread) -> {
      use args2 <- is.bind(instantiate_tcon(tenv, name, span))
      let #(cfields, cres) = args2
      let fields = list.map(arguments, fn(field) { pattern_field(field) })
      use matched <- is.bind(match_constructor_fields(
        fields,
        cfields,
        span,
        with_spread,
      ))
      let #(pairs, remaining) = matched
      use _ignored_remaining <- is.bind(case remaining, with_spread {
        [], _ -> is.ok(Nil)
        _, True -> is.ok(Nil)
        _, False -> is.error("Constructor field mismatch", span)
      })
      use inferred <- is.bind(
        is.map_list(pairs, fn(pair) {
          let #(field, cfield) = pair
          let #(_label, pat) = field
          let #(_clabel, ctype) = cfield
          use tuple <- is.bind(infer_pattern(tenv, pat))
          let #(ptype, scope) = tuple
          use _ignored <- is.bind(unify(ptype, ctype, span))
          is.ok(scope)
        }),
      )
      use cres_applied <- is.bind(type_apply_state(cres))
      let scope =
        list.fold(inferred, dict.new(), fn(acc, scope) {
          dict.merge(acc, scope)
        })
      is.ok(#(cres_applied, scope))
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
  span: g.Span,
  allow_missing: Bool,
) -> is.InferState(
  #(
    List(#(#(option.Option(String), a), #(option.Option(String), b))),
    List(#(option.Option(String), b)),
  ),
) {
  case fields {
    [] ->
      case allow_missing {
        True -> is.ok(#([], cfields))
        False -> is.ok(#([], cfields))
      }
    [field, ..rest] -> {
      let #(label, _value) = field
      case label {
        option.None ->
          case cfields {
            [] -> is.error("Constructor field mismatch", span)
            [cfield, ..ctail] -> {
              use next <- is.bind(match_constructor_fields(
                rest,
                ctail,
                span,
                allow_missing,
              ))
              let #(pairs, remaining) = next
              is.ok(#([#(field, cfield), ..pairs], remaining))
            }
          }
        option.Some(name) -> {
          let #(maybe, remaining) = find_field(name, cfields, [])
          case maybe {
            option.None -> is.error("Unknown field " <> name, span)
            option.Some(cfield) -> {
              use next <- is.bind(match_constructor_fields(
                rest,
                remaining,
                span,
                allow_missing,
              ))
              let #(pairs, remaining2) = next
              is.ok(#([#(field, cfield), ..pairs], remaining2))
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
) -> is.InferState(Nil) {
  let #(_expr, options) = segment
  is.each_list(options, fn(opt) {
    case opt {
      g.SizeValueOption(expr) -> {
        use _ignored <- is.bind(infer_expr(tenv, expr))
        is.ok(Nil)
      }
      _ -> is.ok(Nil)
    }
  })
}

fn infer_bitstring_pat_segment(
  tenv: env.TEnv,
  segment: #(g.Pattern, List(g.BitStringSegmentOption(g.Pattern))),
) -> is.InferState(dict.Dict(String, scheme.Scheme)) {
  let #(pat, options) = segment
  use tuple <- is.bind(infer_pattern(tenv, pat))
  let #(_type, scope) = tuple
  use option_scope <- is.bind(infer_bitstring_pat_options(tenv, options))
  is.ok(dict.merge(scope, option_scope))
}

fn infer_bitstring_pat_options(
  tenv: env.TEnv,
  options: List(g.BitStringSegmentOption(g.Pattern)),
) -> is.InferState(dict.Dict(String, scheme.Scheme)) {
  is.foldl_list(options, dict.new(), fn(acc, opt) {
    case opt {
      g.SizeValueOption(pat) -> {
        use tuple <- is.bind(infer_pattern(tenv, pat))
        let #(_type, scope) = tuple
        is.ok(dict.merge(acc, scope))
      }
      _ -> is.ok(acc)
    }
  })
}

pub fn instantiate_tcon(
  tenv: env.TEnv,
  name: String,
  span: g.Span,
) -> is.InferState(#(List(#(option.Option(String), types.Type)), types.Type)) {
  let env.TEnv(_values, tcons, _types, _aliases, _modules) = tenv
  case dict.get(tcons, name) {
    Error(_) -> is.error("Unknown type constructor: " <> name, span)
    Ok(#(free, cargs, cres)) -> {
      use subst <- is.bind(make_subst_for_free(set.from_list(free), span))
      let args =
        list.map(cargs, fn(field) {
          let #(label, t) = field
          #(label, types.type_apply(subst, t))
        })
      let res = types.type_apply(subst, cres)
      is.ok(#(args, res))
    }
  }
}

pub fn new_type_var(name: String, span: g.Span) -> is.InferState(types.Type) {
  use idx <- is.bind(next_idx_state())
  is.ok(types.Tvar(name <> ":" <> int.to_string(idx), span))
}

pub fn make_subst_for_free(
  vars: set.Set(String),
  span: g.Span,
) -> is.InferState(types.Subst) {
  use mapping <- is.bind(
    is.map_list(set.to_list(vars), fn(id) {
      use new_var <- is.bind(new_type_var(id, span))
      is.ok(#(id, new_var))
    }),
  )
  is.ok(dict.from_list(mapping))
}

pub fn instantiate(
  scheme_: scheme.Scheme,
  span: g.Span,
) -> is.InferState(types.Type) {
  let scheme.Forall(vars, type_) = scheme_
  use subst <- is.bind(make_subst_for_free(vars, span))
  is.ok(types.type_apply(subst, type_))
}

pub fn unify(t1: types.Type, t2: types.Type, span: g.Span) -> is.InferState(Nil) {
  use subst <- is.bind(get_subst_state())
  let left = types.type_apply(subst, t1)
  let right = types.type_apply(subst, t2)
  case left, right {
    types.Tvar(var, _), t -> var_bind(var, t, span)
    t, types.Tvar(var, _) -> var_bind(var, t, span)
    types.Tcon(a, _), types.Tcon(b, _) ->
      case a == b {
        True -> is.ok(Nil)
        False ->
          is.error("Incompatible concrete types: " <> a <> " vs " <> b, span)
      }
    types.Tapp(t1a, a1, _), types.Tapp(t2a, a2, _) ->
      case list.length(a1) == list.length(a2) {
        True -> {
          use _ignored <- is.bind(unify(t1a, t2a, span))
          use subst <- is.bind(get_subst_state())
          let left = list.map(a1, fn(arg) { types.type_apply(subst, arg) })
          let right = list.map(a2, fn(arg) { types.type_apply(subst, arg) })
          is.each_list(list.zip(left, right), fn(pair) {
            let #(l, r) = pair
            unify(l, r, span)
          })
        }
        False ->
          is.error(
            "Incompatible type application arity: "
              <> types.type_to_string(t1)
              <> " vs "
              <> types.type_to_string(t2),
            span,
          )
      }
    types.Tfn(args1, res1, _), types.Tfn(args2, res2, _) ->
      case list.length(args1) == list.length(args2) {
        True -> {
          use _ignored <- is.bind(
            is.each_list(list.zip(args1, args2), fn(args) {
              let #(left, right) = args
              unify(left, right, span)
            }),
          )
          use subst <- is.bind(get_subst_state())
          let left = types.type_apply(subst, res1)
          let right = types.type_apply(subst, res2)
          unify(left, right, span)
        }
        False ->
          is.error(
            "Incompatible function arity: "
              <> types.type_to_string(t1)
              <> " vs "
              <> types.type_to_string(t2),
            span,
          )
      }
    types.Ttuple(args1, _), types.Ttuple(args2, _) ->
      case list.length(args1) == list.length(args2) {
        True ->
          is.each_list(list.zip(args1, args2), fn(args) {
            let #(left, right) = args
            unify(left, right, span)
          })
        False ->
          is.error(
            "Incompatible tuple arity: "
              <> types.type_to_string(t1)
              <> " vs "
              <> types.type_to_string(t2),
            span,
          )
      }
    _, _ ->
      is.error(
        "Incompatible types: "
          <> types.type_to_string(t1)
          <> " vs "
          <> types.type_to_string(t2),
        span,
      )
  }
}

pub fn var_bind(
  var: String,
  type_: types.Type,
  span: g.Span,
) -> is.InferState(Nil) {
  case type_ {
    types.Tvar(v, _) ->
      case var == v {
        True -> is.ok(Nil)
        False -> {
          use _ignored <- is.bind(put_subst_state(one_subst(var, type_)))
          is.ok(Nil)
        }
      }
    _ ->
      case set.contains(types.type_free(type_), var) {
        True ->
          is.error(
            "Cycle found while unifying type with type variable: " <> var,
            span,
          )
        False -> {
          use _ignored <- is.bind(put_subst_state(one_subst(var, type_)))
          is.ok(Nil)
        }
      }
  }
}

pub fn one_subst(var: String, type_: types.Type) -> types.Subst {
  dict.from_list([#(var, type_)])
}

fn tenv_apply_state(tenv: env.TEnv) -> is.InferState(env.TEnv) {
  is.apply_with(env.apply, tenv)
}

fn type_apply_state(type_: types.Type) -> is.InferState(types.Type) {
  is.apply_with(types.type_apply, type_)
}

fn scope_apply_state(
  scope: dict.Dict(String, scheme.Scheme),
) -> is.InferState(dict.Dict(String, scheme.Scheme)) {
  is.apply_with(env.scope_apply, scope)
}

fn get_subst_state() -> is.InferState(dict.Dict(String, types.Type)) {
  is.lift(state.get_subst())
}

fn put_subst_state(
  new_subst: dict.Dict(String, types.Type),
) -> is.InferState(Nil) {
  is.lift(state.put_subst(new_subst))
}

fn reset_subst_state(
  new_subst: dict.Dict(String, types.Type),
) -> is.InferState(dict.Dict(String, types.Type)) {
  is.lift(state.reset_subst(new_subst))
}

fn next_idx_state() -> is.InferState(Int) {
  is.lift(state.next_idx())
}

fn tuple_index_type(
  args: List(types.Type),
  index: Int,
  span: g.Span,
) -> is.InferState(types.Type) {
  case args, index {
    [], _ -> is.error("Tuple index out of range", span)
    [head, ..], 0 -> type_apply_state(head)
    [_head, ..tail], _ -> tuple_index_type(tail, index - 1, span)
  }
}

fn tuple_index_vars(count: Int, span: g.Span) -> is.InferState(List(types.Type)) {
  case count <= 0 {
    True -> is.ok([])
    False -> {
      use rest <- is.bind(tuple_index_vars(count - 1, span))
      use v <- is.bind(new_type_var("tuple_item", span))
      is.ok([v, ..rest])
    }
  }
}

fn lookup_constructor_field(
  label: String,
  cfields: List(#(option.Option(String), types.Type)),
  span: g.Span,
) -> is.InferState(types.Type) {
  case cfields {
    [] -> is.error("Unknown field " <> label, span)
    [field, ..rest] -> {
      let #(maybe, t) = field
      case maybe {
        option.Some(name) ->
          case name == label {
            True -> is.ok(t)
            False -> lookup_constructor_field(label, rest, span)
          }
        option.None -> lookup_constructor_field(label, rest, span)
      }
    }
  }
}
