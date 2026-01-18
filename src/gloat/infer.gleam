import glance as g
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/set
import gleam/string
import gloat/env
import gloat/exhaustive
import gloat/gleam_types
import gloat/infer_state as is
import gloat/literals
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
    g.Int(span, value) -> {
      case literals.parse_int_literal(value) {
        Ok(_) -> is.ok(types.Tcon("Int", span))
        Error(_) -> is.error("Invalid int literal", span)
      }
    }

    g.Float(span, value) -> {
      let normalized =
        string.replace(value, "_", "")
        <> case string.ends_with(value, ".") {
          True -> "0"
          False -> ""
        }
      case float.parse(normalized) {
        Ok(_) -> is.ok(types.Tcon("Float", span))
        Error(_) -> is.error("Invalid float literal", span)
      }
    }

    g.String(span, _value) -> is.ok(types.Tcon("String", span))

    g.Variable(span, name) ->
      case env.resolve(tenv, name) {
        Ok(scheme_) -> instantiate(scheme_, span)
        Error(_) -> is.error("Variable not found in scope: " <> name, span)
      }

    g.NegateInt(span, value) -> {
      let _loc = gleam_types.loc_from_span(span)
      use value_type <- is.bind(infer_expr_inner(tenv, value))
      use _ignored <- is.bind(unify(value_type, types.Tcon("Int", span), span))
      is.ok(types.Tcon("Int", span))
    }

    g.NegateBool(span, value) -> {
      let _loc = gleam_types.loc_from_span(span)
      use value_type <- is.bind(infer_expr_inner(tenv, value))
      use _ignored <- is.bind(unify(value_type, types.Tcon("Bool", span), span))
      is.ok(types.Tcon("Bool", span))
    }

    g.Block(span, statements) -> infer_block(tenv, statements, span)

    g.Panic(span, message) -> {
      let _loc = gleam_types.loc_from_span(span)
      use _ignored <- is.bind(case message {
        option.Some(expr) -> {
          use msg_type <- is.bind(infer_expr_inner(tenv, expr))
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
          use msg_type <- is.bind(infer_expr_inner(tenv, expr))
          unify(msg_type, types.Tcon("String", span), span)
        }
        option.None -> is.ok(Nil)
      })
      new_type_var("todo", span)
    }

    g.Tuple(span, items) -> {
      use item_types <- is.bind(
        is.map_list(items, fn(item) { infer_expr_inner(tenv, item) }),
      )
      is.ok(types.Ttuple(item_types, span))
    }

    g.TupleIndex(span, target, index) -> {
      use target_type <- is.bind(infer_expr_inner(tenv, target))
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
        is.map_list(items, fn(item) { infer_expr_inner(tenv, item) }),
      )
      use _ignored <- is.bind(
        is.each_list(item_types, fn(item_type) {
          unify(item_type, elem_type, span)
        }),
      )
      case tail {
        option.None -> type_apply_state(list_type)
        option.Some(tail_expr) -> {
          use tail_type <- is.bind(infer_expr_inner(tenv, tail_expr))
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
          use _ignored2 <- is.bind(infer_expr_inner(tenv, expr))
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
          use msg_type <- is.bind(infer_expr_inner(tenv, expr))
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
        option.Some(expr) -> infer_expr_inner(tenv, expr)
        option.None -> is.ok(types.Tcon("()", span))
      }
    }

    g.RecordUpdate(span, _module, name, record, fields) -> {
      let _loc = gleam_types.loc_from_span(span)
      use args2 <- is.bind(instantiate_tcon(tenv, name, span))
      let #(cfields, cres) = args2
      use record_type <- is.bind(infer_expr_inner(tenv, record))
      use applied_record <- is.bind(type_apply_state(record_type))
      case applied_record {
        types.Tvar(_, _) -> {
          use _ignored <- is.bind(unify(record_type, cres, span))
          type_apply_state(cres)
        }
        _ -> {
          use record_tcon <- is.bind(
            is.from_result(types.tcon_and_args(applied_record, [], span)),
          )
          let #(record_tname, record_targs) = record_tcon
          use constructor_tcon <- is.bind(
            is.from_result(types.tcon_and_args(cres, [], span)),
          )
          let #(constructor_tname, _constructor_targs) = constructor_tcon
          use _ignored <- is.bind(case record_tname == constructor_tname {
            True -> is.ok(Nil)
            False -> is.error("Record update type mismatch", span)
          })
          let env.TEnv(
            _values,
            tcons,
            _types,
            _aliases,
            _modules,
            _params,
            _type_names,
            _refinements,
          ) = tenv
          use constructor_def <- is.bind(case dict.get(tcons, name) {
            Ok(value) -> is.ok(value)
            Error(_) -> is.error("Unknown constructor " <> name, span)
          })
          let #(free, raw_fields, _cres) = constructor_def
          let record_subst = dict.from_list(list.zip(free, record_targs))
          let record_fields =
            list.map(raw_fields, fn(field) {
              let #(label, t) = field
              #(label, types.type_apply(record_subst, t))
            })
          let updated_labels =
            list.map(fields, fn(field) {
              let g.RecordUpdateField(label, _item) = field
              label
            })
          use _ignored2 <- is.bind(
            is.each_list(fields, fn(field) {
              let g.RecordUpdateField(label, item) = field
              use ctype <- is.bind(lookup_constructor_field(
                label,
                cfields,
                span,
              ))
              let expr = case item {
                option.Some(expr) -> expr
                option.None -> g.Variable(span, label)
              }
              use expr_type <- is.bind(infer_expr_inner(tenv, expr))
              unify(expr_type, ctype, span)
            }),
          )
          use _ignored3 <- is.bind(
            is.each_list(record_fields, fn(field) {
              let #(maybe_label, record_type) = field
              case maybe_label {
                option.None -> is.ok(Nil)
                option.Some(label) ->
                  case list.contains(updated_labels, label) {
                    True -> is.ok(Nil)
                    False -> {
                      use ctype <- is.bind(lookup_constructor_field(
                        label,
                        cfields,
                        span,
                      ))
                      unify(record_type, ctype, span)
                    }
                  }
              }
            }),
          )
          type_apply_state(cres)
        }
      }
    }

    g.FieldAccess(span, container, label) -> {
      case container {
        g.Variable(_span, name) ->
          case env.resolve_refinement(tenv, name) {
            Ok(constructor_name) -> {
              use target_type <- is.bind(infer_expr_inner(tenv, container))
              use applied_target <- is.bind(type_apply_state(target_type))
              case applied_target {
                types.Tvar(_, _) ->
                  is.error("Record field access requires known type", span)
                _ ->
                  field_type_for_constructor(
                    tenv,
                    constructor_name,
                    applied_target,
                    label,
                    span,
                  )
              }
            }
            Error(_) -> {
              let module_value = case env.resolve_module(tenv, name) {
                Ok(module_key) ->
                  case env.resolve(tenv, module_key <> "/" <> label) {
                    Ok(scheme_) -> option.Some(instantiate(scheme_, span))
                    Error(_) ->
                      case module_key == "gleam" {
                        True ->
                          case env.resolve(tenv, label) {
                            Ok(scheme_) ->
                              option.Some(instantiate(scheme_, span))
                            Error(_) -> option.None
                          }
                        False -> option.None
                      }
                  }
                Error(_) -> option.None
              }
              case module_value {
                option.Some(result) -> result
                option.None ->
                  case env.resolve(tenv, name) {
                    Ok(_) ->
                      infer_record_field_access(tenv, span, container, label)
                    Error(_) ->
                      case env.resolve_module(tenv, name) {
                        Ok(module_key) ->
                          is.error(
                            "Unknown module value "
                              <> module_key
                              <> "/"
                              <> label,
                            span,
                          )
                        Error(_) ->
                          infer_record_field_access(
                            tenv,
                            span,
                            container,
                            label,
                          )
                      }
                  }
              }
            }
          }
        _ -> infer_record_field_access(tenv, span, container, label)
      }
    }

    g.Fn(span, params, return_annotation, body) -> {
      use inferred <- is.bind(
        is.map_list(params, fn(param) {
          let g.FnParameter(name, annotation) = param
          case annotation {
            option.Some(type_expr) ->
              case gleam_types.type_(tenv, type_expr) {
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
          case gleam_types.type_(tenv, type_expr) {
            Ok(type_) -> unify(body_type, type_, span)
            Error(gleam_types.Unsupported(name)) ->
              is.error("Unsupported return annotation: " <> name, span)
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

    [g.Expression(expr)] -> infer_expr_inner(tenv, expr)

    [g.Expression(expr), ..rest] -> {
      use _ignored <- is.bind(infer_expr_inner(tenv, expr))
      infer_block(tenv, rest, span)
    }

    [g.Assignment(assign_span, kind, pat, annotation, value), ..rest] ->
      infer_assignment(tenv, kind, pat, annotation, value, rest, assign_span)

    [g.Assert(span, expression, message), ..rest] -> {
      use expr_type <- is.bind(infer_expr_inner(tenv, expression))
      use _ignored <- is.bind(unify(expr_type, types.Tcon("Bool", span), span))
      use _ignored2 <- is.bind(case message {
        option.Some(msg) -> {
          use msg_type <- is.bind(infer_expr_inner(tenv, msg))
          unify(msg_type, types.Tcon("String", span), span)
        }
        option.None -> is.ok(Nil)
      })
      case rest {
        [] -> is.ok(types.Tcon("Nil", span))
        _ -> infer_block(tenv, rest, span)
      }
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
  use callback_result <- is.bind(new_type_var("use_callback", span))
  let callback_type = types.Tfn(arg_types, callback_result, span)
  use result_var <- is.bind(new_type_var("use_result", span))
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
  use _ignored2 <- is.bind(unify(callback_result, rest_type, block_span))
  type_apply_state(result_var)
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
      let has_labels =
        list.any(args, fn(arg) {
          let #(label, _expr) = arg
          label != option.None
        })
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
      let ordered_arg_types = case has_labels {
        True ->
          case resolve_call_params(tenv, target) {
            Ok(params) ->
              case
                reorder_use_call_args(
                  params,
                  arguments,
                  arg_types,
                  callback_type,
                )
              {
                Ok(ordered) -> ordered
                Error(_) -> list.append(arg_types, [callback_type])
              }
            Error(_) -> list.append(arg_types, [callback_type])
          }
        False -> list.append(arg_types, [callback_type])
      }
      use target_type_applied <- is.bind(type_apply_state(target_type))
      unify(
        target_type_applied,
        types.Tfn(ordered_arg_types, result_var, span),
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
          case gleam_types.type_(tenv, type_expr) {
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
  kind: g.AssignmentKind,
  pat: g.Pattern,
  annotation: option.Option(g.Type),
  value: g.Expression,
  rest: List(g.Statement),
  span: g.Span,
) -> is.InferState(types.Type) {
  use value_type <- is.bind(infer_expr_inner(tenv, value))
  use _ignored2 <- is.bind(case annotation {
    option.Some(type_expr) ->
      case gleam_types.type_(tenv, type_expr) {
        Ok(type_) -> unify(type_, value_type, span)
        Error(_) -> is.error("Unsupported annotation", span)
      }
    option.None -> is.ok(Nil)
  })
  let is_empty = case rest {
    [] -> True
    _ -> False
  }
  case pat {
    g.PatternVariable(_span, name) -> {
      use applied_env <- is.bind(tenv_apply_state(tenv))
      let scheme_ = env.generalize(applied_env, value_type)
      let refinements = assignment_refinements(tenv, kind, pat, value)
      let bound_env =
        env.with_refinements(
          env.with_type(applied_env, name, scheme_),
          refinements,
        )
      case is_empty {
        True -> type_apply_state(value_type)
        False -> infer_block(bound_env, rest, span)
      }
    }
    _ -> {
      use tuple <- is.bind(infer_pattern(tenv, pat))
      let #(type_, scope) = tuple
      use _ignored <- is.bind(unify(type_, value_type, span))
      use scope_applied <- is.bind(scope_apply_state(scope))
      let refinements = assignment_refinements(tenv, kind, pat, value)
      let bound_env =
        env.with_refinements(env.with_scope(tenv, scope_applied), refinements)
      case is_empty {
        True -> type_apply_state(value_type)
        False -> infer_block(bound_env, rest, span)
      }
    }
  }
}

fn infer_call(
  tenv: env.TEnv,
  span: g.Span,
  function: g.Expression,
  arguments: List(g.Field(g.Expression)),
) -> is.InferState(types.Type) {
  use _ignored <- is.bind(new_type_var("call", span))
  let args = list.map(arguments, fn(field) { field_expr(field) })
  let has_labels =
    list.any(args, fn(arg) {
      let #(label, _expr) = arg
      label != option.None
    })
  case has_labels, constructor_name(tenv, function) {
    True, option.Some(_) -> infer_constructor_call(tenv, span, function, args)
    _, _ -> {
      let exprs = case has_labels {
        True ->
          case resolve_call_params(tenv, function) {
            Ok(params) ->
              case reorder_call_args(params, arguments) {
                Ok(ordered) -> ordered
                Error(_) ->
                  list.map(args, fn(arg) {
                    let #(_label, expr) = arg
                    expr
                  })
              }
            Error(_) ->
              list.map(args, fn(arg) {
                let #(_label, expr) = arg
                expr
              })
          }
        False ->
          list.map(args, fn(arg) {
            let #(_label, expr) = arg
            expr
          })
      }
      use target_type <- is.bind(infer_expr_inner(tenv, function))
      use arg_types <- is.bind(
        is.map_list(exprs, fn(expr) {
          use arg_tenv <- is.bind(tenv_apply_state(tenv))
          infer_expr_inner(arg_tenv, expr)
        }),
      )
      use target_type_applied <- is.bind(type_apply_state(target_type))
      apply_call_args(target_type_applied, arg_types, span)
    }
  }
}

fn reorder_call_args(
  params: List(option.Option(String)),
  arguments: List(g.Field(g.Expression)),
) -> Result(List(g.Expression), Nil) {
  let labelled =
    list.fold(arguments, dict.new(), fn(acc, field) {
      let #(_label, expr) = field_expr(field)
      case arg_label(field) {
        option.Some(label) -> dict.insert(acc, label, expr)
        option.None -> acc
      }
    })
  let unlabelled =
    list.fold(arguments, [], fn(acc, field) {
      let #(_label, expr) = field_expr(field)
      case arg_label(field) {
        option.Some(_) -> acc
        option.None -> [expr, ..acc]
      }
    })
    |> list.reverse
  reorder_call_args_inner(params, labelled, unlabelled, [])
}

fn reorder_call_args_inner(
  params: List(option.Option(String)),
  labelled: dict.Dict(String, g.Expression),
  unlabelled: List(g.Expression),
  acc: List(g.Expression),
) -> Result(List(g.Expression), Nil) {
  case params {
    [] -> Ok(list.reverse(acc))
    [param, ..rest] ->
      case param {
        option.Some(label) ->
          case dict.get(labelled, label) {
            Ok(expr) ->
              reorder_call_args_inner(rest, labelled, unlabelled, [expr, ..acc])
            Error(_) ->
              case unlabelled {
                [expr, ..tail] ->
                  reorder_call_args_inner(rest, labelled, tail, [expr, ..acc])
                [] -> Error(Nil)
              }
          }
        option.None ->
          case unlabelled {
            [expr, ..tail] ->
              reorder_call_args_inner(rest, labelled, tail, [expr, ..acc])
            [] -> Error(Nil)
          }
      }
  }
}

fn reorder_use_call_args(
  params: List(option.Option(String)),
  arguments: List(g.Field(g.Expression)),
  arg_types: List(types.Type),
  callback_type: types.Type,
) -> Result(List(types.Type), Nil) {
  let labelled =
    list.fold(list.zip(arguments, arg_types), dict.new(), fn(acc, pair) {
      let #(field, arg_type) = pair
      case arg_label(field) {
        option.Some(label) -> dict.insert(acc, label, arg_type)
        option.None -> acc
      }
    })
  let unlabelled =
    list.fold(list.zip(arguments, arg_types), [], fn(acc, pair) {
      let #(field, arg_type) = pair
      case arg_label(field) {
        option.Some(_) -> acc
        option.None -> [arg_type, ..acc]
      }
    })
    |> list.reverse
  reorder_use_call_args_inner(
    params,
    labelled,
    unlabelled,
    callback_type,
    False,
    [],
  )
}

fn reorder_use_call_args_inner(
  params: List(option.Option(String)),
  labelled: dict.Dict(String, types.Type),
  unlabelled: List(types.Type),
  callback_type: types.Type,
  used_callback: Bool,
  acc: List(types.Type),
) -> Result(List(types.Type), Nil) {
  case params {
    [] ->
      case list.is_empty(unlabelled) && dict.size(labelled) == 0 {
        True -> Ok(list.reverse(acc))
        False -> Error(Nil)
      }
    [param, ..rest] ->
      case param {
        option.Some(label) ->
          case dict.get(labelled, label) {
            Ok(arg_type) ->
              reorder_use_call_args_inner(
                rest,
                dict.delete(labelled, label),
                unlabelled,
                callback_type,
                used_callback,
                [arg_type, ..acc],
              )
            Error(_) ->
              case used_callback {
                True -> Error(Nil)
                False ->
                  reorder_use_call_args_inner(
                    rest,
                    labelled,
                    unlabelled,
                    callback_type,
                    True,
                    [callback_type, ..acc],
                  )
              }
          }
        option.None ->
          case unlabelled {
            [arg_type, ..tail] ->
              reorder_use_call_args_inner(
                rest,
                labelled,
                tail,
                callback_type,
                used_callback,
                [arg_type, ..acc],
              )
            [] ->
              case used_callback {
                True -> Error(Nil)
                False ->
                  reorder_use_call_args_inner(
                    rest,
                    labelled,
                    unlabelled,
                    callback_type,
                    True,
                    [callback_type, ..acc],
                  )
              }
          }
      }
  }
}

fn apply_call_args(
  target_type: types.Type,
  arg_types: List(types.Type),
  span: g.Span,
) -> is.InferState(types.Type) {
  case arg_types {
    [] -> {
      use result_var <- is.bind(new_type_var("result", span))
      use _ignored <- is.bind(unify(
        target_type,
        types.Tfn([], result_var, span),
        span,
      ))
      type_apply_state(result_var)
    }
    _ ->
      case target_type {
        types.Tfn(fn_args, fn_result, _fn_span) -> {
          let result_name = case fn_result {
            types.Tvar(name, _) -> base_var_name(name)
            _ -> "result"
          }
          use result_var <- is.bind(new_type_var(result_name, span))
          let arg_count = list.length(arg_types)
          let fn_count = list.length(fn_args)
          case arg_count <= fn_count {
            True -> {
              let #(prefix, remaining) = split_list_at(fn_args, arg_count)
              use _ignored <- is.bind(
                is.each_list(list.zip(prefix, arg_types), fn(pair) {
                  let #(fn_arg, call_arg) = pair
                  unify(fn_arg, call_arg, span)
                }),
              )
              use _ignored2 <- is.bind(unify(fn_result, result_var, span))
              case remaining {
                [] -> type_apply_state(result_var)
                _ -> is.ok(types.Tfn(remaining, result_var, span))
              }
            }
            False -> {
              let #(prefix, rest_args) = split_list_at(arg_types, fn_count)
              use _ignored <- is.bind(
                is.each_list(list.zip(fn_args, prefix), fn(pair) {
                  let #(fn_arg, call_arg) = pair
                  unify(fn_arg, call_arg, span)
                }),
              )
              use _ignored2 <- is.bind(unify(fn_result, result_var, span))
              use result_applied <- is.bind(type_apply_state(result_var))
              apply_call_args(result_applied, rest_args, span)
            }
          }
        }
        _ -> {
          use result_var <- is.bind(new_type_var("result", span))
          use _ignored <- is.bind(unify(
            target_type,
            types.Tfn(arg_types, result_var, span),
            span,
          ))
          type_apply_state(result_var)
        }
      }
  }
}

fn split_list_at(items: List(a), count: Int) -> #(List(a), List(a)) {
  case count <= 0, items {
    True, _ -> #([], items)
    False, [] -> #([], [])
    False, [first, ..rest] -> {
      let #(prefix, remaining) = split_list_at(rest, count - 1)
      #([first, ..prefix], remaining)
    }
  }
}

fn base_var_name(name: String) -> String {
  let parts = string.split(name, ":")
  case parts {
    [] -> name
    [_] -> name
    _ -> {
      let trimmed = list.reverse(parts)
      let trimmed = list.drop(trimmed, 1)
      let trimmed = list.reverse(trimmed)
      string.join(trimmed, with: ":")
    }
  }
}

fn constructor_name(
  tenv: env.TEnv,
  function: g.Expression,
) -> option.Option(String) {
  let name = case function {
    g.Variable(_span, name) -> option.Some(name)
    g.FieldAccess(_span, g.Variable(_, module_name), name) ->
      case env.resolve_module(tenv, module_name) {
        Ok(module_key) -> option.Some(module_key <> "/" <> name)
        Error(_) -> option.None
      }
    _ -> option.None
  }
  case name {
    option.None -> option.None
    option.Some(name) -> {
      let env.TEnv(
        _values,
        tcons,
        _types,
        _aliases,
        _modules,
        _params,
        _type_names,
        _refinements,
      ) = tenv
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
    g.FieldAccess(_span, g.Variable(_, module_name), name) ->
      case env.resolve_module(tenv, module_name) {
        Ok(module_key) -> Ok(module_key <> "/" <> name)
        Error(_) -> Error(Nil)
      }
    _ -> Error(Nil)
  }
  case name {
    Error(_) -> is.error("Labelled arguments require constructor", span)
    Ok(name) -> {
      use args2 <- is.bind(instantiate_tcon(tenv, name, span))
      let #(cfields, cres) = args2
      let has_labels =
        list.any(fields, fn(field) {
          let #(label, _expr) = field
          label != option.None
        })
      let ordered_fields = case has_labels {
        True ->
          case reorder_constructor_fields(cfields, fields) {
            Ok(exprs) -> list.map(exprs, fn(expr) { #(option.None, expr) })
            Error(_) -> fields
          }
        False -> fields
      }
      use matched <- is.bind(match_constructor_fields(
        ordered_fields,
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
          use expr_type <- is.bind(infer_expr_inner(tenv, expr))
          unify(expr_type, ctype, span)
        }),
      )
      type_apply_state(cres)
    }
  }
}

fn reorder_constructor_fields(
  cfields: List(#(option.Option(String), types.Type)),
  fields: List(#(option.Option(String), g.Expression)),
) -> Result(List(g.Expression), Nil) {
  let labelled =
    list.fold(fields, dict.new(), fn(acc, field) {
      let #(label, expr) = field
      case label {
        option.Some(name) -> dict.insert(acc, name, expr)
        option.None -> acc
      }
    })
  let unlabelled =
    list.fold(fields, [], fn(acc, field) {
      let #(label, expr) = field
      case label {
        option.Some(_) -> acc
        option.None -> [expr, ..acc]
      }
    })
    |> list.reverse
  reorder_constructor_fields_inner(cfields, labelled, unlabelled, [])
}

fn reorder_constructor_fields_inner(
  cfields: List(#(option.Option(String), types.Type)),
  labelled: dict.Dict(String, g.Expression),
  unlabelled: List(g.Expression),
  acc: List(g.Expression),
) -> Result(List(g.Expression), Nil) {
  case cfields {
    [] ->
      case list.is_empty(unlabelled) && dict.size(labelled) == 0 {
        True -> Ok(list.reverse(acc))
        False -> Error(Nil)
      }
    [cfield, ..rest] -> {
      let #(maybe_label, _ctype) = cfield
      case maybe_label {
        option.Some(label) ->
          case dict.get(labelled, label) {
            Ok(expr) ->
              reorder_constructor_fields_inner(
                rest,
                dict.delete(labelled, label),
                unlabelled,
                [expr, ..acc],
              )
            Error(_) ->
              case unlabelled {
                [expr, ..tail] ->
                  reorder_constructor_fields_inner(rest, labelled, tail, [
                    expr,
                    ..acc
                  ])
                [] -> Error(Nil)
              }
          }
        option.None ->
          case unlabelled {
            [expr, ..tail] ->
              reorder_constructor_fields_inner(rest, labelled, tail, [
                expr,
                ..acc
              ])
            [] -> Error(Nil)
          }
      }
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
      let subject_expr = case subjects {
        [subject] -> option.Some(subject)
        _ -> option.None
      }
      use subject_types <- is.bind(
        is.map_list(subjects, fn(subject) { infer_expr_inner(tenv, subject) }),
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
          let refinements = case subject_expr {
            option.Some(subject) -> constructor_refinements(tenv, subject, pat)
            option.None -> dict.new()
          }
          let bound_env =
            env.with_refinements(
              env.with_scope(tenv, scope_applied),
              refinements,
            )
          use _ignored_guard <- is.bind(case guard {
            option.Some(expr) -> {
              use guard_type <- is.bind(infer_expr_inner(bound_env, expr))
              unify(guard_type, types.Tcon("Bool", span), span)
            }
            option.None -> is.ok(Nil)
          })
          use body_type <- is.bind(infer_expr_inner(bound_env, body))
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

fn pattern_constructor_refinement(
  tenv: env.TEnv,
  pat: g.Pattern,
) -> option.Option(#(String, option.Option(String))) {
  case pat {
    g.PatternVariant(_span, module, name, _arguments, _with_spread) ->
      case resolve_pattern_constructor(tenv, module, name) {
        Ok(constructor_name) -> option.Some(#(constructor_name, option.None))
        Error(_) -> option.None
      }

    g.PatternAssignment(_span, inner, alias) ->
      case inner {
        g.PatternVariant(_span, module, name, _arguments, _with_spread) ->
          case resolve_pattern_constructor(tenv, module, name) {
            Ok(constructor_name) ->
              option.Some(#(constructor_name, option.Some(alias)))
            Error(_) -> option.None
          }
        _ -> option.None
      }

    _ -> option.None
  }
}

fn constructor_refinements(
  tenv: env.TEnv,
  subject: g.Expression,
  pat: g.Pattern,
) -> dict.Dict(String, String) {
  case pattern_constructor_refinement(tenv, pat) {
    option.None -> dict.new()
    option.Some(#(constructor_name, alias)) -> {
      let refinements = case subject_refinement_name(subject) {
        option.Some(name) -> dict.from_list([#(name, constructor_name)])
        option.None -> dict.new()
      }
      case alias {
        option.Some(alias_name) ->
          dict.insert(refinements, alias_name, constructor_name)
        option.None -> refinements
      }
    }
  }
}

fn assignment_refinements(
  tenv: env.TEnv,
  kind: g.AssignmentKind,
  pat: g.Pattern,
  value: g.Expression,
) -> dict.Dict(String, String) {
  case kind {
    g.LetAssert(_message) ->
      case pattern_constructor_refinement(tenv, pat) {
        option.None -> dict.new()
        option.Some(#(constructor_name, alias)) -> {
          let refinements = case subject_refinement_name(value) {
            option.Some(name) -> dict.from_list([#(name, constructor_name)])
            option.None -> dict.new()
          }
          case alias {
            option.Some(alias_name) ->
              dict.insert(refinements, alias_name, constructor_name)
            option.None -> refinements
          }
        }
      }
    g.Let ->
      case pat {
        g.PatternVariable(_span, name) ->
          case value {
            g.Call(_call_span, function, _arguments) ->
              case constructor_name(tenv, function) {
                option.Some(constructor_name) ->
                  dict.from_list([#(name, constructor_name)])
                option.None -> dict.new()
              }
            _ -> dict.new()
          }
        _ -> dict.new()
      }
  }
}

fn subject_refinement_name(subject: g.Expression) -> option.Option(String) {
  case subject {
    g.Variable(_span, name) -> option.Some(name)
    g.Echo(_span, value, _message) ->
      case value {
        option.Some(inner) -> subject_refinement_name(inner)
        option.None -> option.None
      }
    _ -> option.None
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
      use left_type <- is.bind(infer_expr_inner(tenv, left))
      use right_type <- is.bind(infer_expr_inner(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("Bool", span), span))
      use _ignored2 <- is.bind(unify(right_type, types.Tcon("Bool", span), span))
      is.ok(types.Tcon("Bool", span))
    }

    g.Eq | g.NotEq -> {
      use left_type <- is.bind(infer_expr_inner(tenv, left))
      use right_type <- is.bind(infer_expr_inner(tenv, right))
      use _ignored <- is.bind(unify(left_type, right_type, span))
      is.ok(types.Tcon("Bool", span))
    }

    g.LtInt | g.LtEqInt | g.GtInt | g.GtEqInt -> {
      use left_type <- is.bind(infer_expr_inner(tenv, left))
      use right_type <- is.bind(infer_expr_inner(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("Int", span), span))
      use _ignored2 <- is.bind(unify(right_type, types.Tcon("Int", span), span))
      is.ok(types.Tcon("Bool", span))
    }

    g.LtFloat | g.LtEqFloat | g.GtFloat | g.GtEqFloat -> {
      use left_type <- is.bind(infer_expr_inner(tenv, left))
      use right_type <- is.bind(infer_expr_inner(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("Float", span), span))
      use _ignored2 <- is.bind(unify(
        right_type,
        types.Tcon("Float", span),
        span,
      ))
      is.ok(types.Tcon("Bool", span))
    }

    g.AddInt | g.SubInt | g.MultInt | g.DivInt | g.RemainderInt -> {
      use left_type <- is.bind(infer_expr_inner(tenv, left))
      use right_type <- is.bind(infer_expr_inner(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("Int", span), span))
      use _ignored2 <- is.bind(unify(right_type, types.Tcon("Int", span), span))
      is.ok(types.Tcon("Int", span))
    }

    g.AddFloat | g.SubFloat | g.MultFloat | g.DivFloat -> {
      use left_type <- is.bind(infer_expr_inner(tenv, left))
      use right_type <- is.bind(infer_expr_inner(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("Float", span), span))
      use _ignored2 <- is.bind(unify(
        right_type,
        types.Tcon("Float", span),
        span,
      ))
      is.ok(types.Tcon("Float", span))
    }

    g.Concatenate -> {
      use left_type <- is.bind(infer_expr_inner(tenv, left))
      use right_type <- is.bind(infer_expr_inner(tenv, right))
      use _ignored <- is.bind(unify(left_type, types.Tcon("String", span), span))
      use _ignored2 <- is.bind(unify(
        right_type,
        types.Tcon("String", span),
        span,
      ))
      is.ok(types.Tcon("String", span))
    }

    g.Pipe -> infer_expr_inner(tenv, pipe_to_call(tenv, span, left, right))
  }
}

fn pipe_to_call(
  tenv: env.TEnv,
  span: g.Span,
  left: g.Expression,
  right: g.Expression,
) -> g.Expression {
  case right {
    g.Echo(echo_span, option.None, message) ->
      g.Echo(echo_span, option.Some(left), message)
    g.FnCapture(call_span, option.None, function, args_before, args_after) -> {
      let piped = g.UnlabelledField(left)
      let before = list.append(args_before, [piped])
      let piped_args = list.append(before, args_after)
      g.Call(call_span, function, piped_args)
    }
    g.Call(call_span, function, arguments) -> {
      let piped = g.UnlabelledField(left)
      let has_labels =
        list.any(arguments, fn(arg) { arg_label(arg) != option.None })
      let piped_args = case has_labels {
        True ->
          case insert_piped_by_labels(tenv, function, arguments, piped) {
            Ok(piped_args) -> piped_args
            Error(_) -> list.append(arguments, [piped])
          }
        False ->
          case resolve_call_params(tenv, function) {
            Ok(params) ->
              case list.length(arguments) < list.length(params) {
                True -> [piped, ..arguments]
                False -> list.append(arguments, [piped])
              }
            Error(_) -> list.append(arguments, [piped])
          }
      }
      g.Call(call_span, function, piped_args)
    }
    _ -> g.Call(span, right, [g.UnlabelledField(left)])
  }
}

fn insert_piped_by_labels(
  tenv: env.TEnv,
  function: g.Expression,
  arguments: List(g.Field(g.Expression)),
  piped: g.Field(g.Expression),
) -> Result(List(g.Field(g.Expression)), Nil) {
  case resolve_call_params(tenv, function) {
    Ok(params) -> {
      let provided = provided_labels(arguments)
      case missing_param_index(params, provided, 0) {
        Ok(pipe_index) -> {
          let label_map = label_index_map(params)
          case split_at_param_index(arguments, label_map, pipe_index, []) {
            Ok(#(prefix, rest)) -> Ok(list.append(prefix, [piped, ..rest]))
            Error(_) -> Error(Nil)
          }
        }
        Error(_) -> Error(Nil)
      }
    }
    Error(_) -> Error(Nil)
  }
}

fn resolve_call_params(
  tenv: env.TEnv,
  function: g.Expression,
) -> Result(List(option.Option(String)), Nil) {
  case function {
    g.Variable(_span, name) -> env.resolve_params(tenv, name)
    g.FieldAccess(_span, g.Variable(_, module_name), label) ->
      case env.resolve_module(tenv, module_name) {
        Ok(module_key) -> env.resolve_params(tenv, module_key <> "/" <> label)
        Error(_) -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn provided_labels(arguments: List(g.Field(g.Expression))) -> set.Set(String) {
  list.fold(arguments, set.new(), fn(acc, field) {
    case arg_label(field) {
      option.Some(label) -> set.insert(acc, label)
      option.None -> acc
    }
  })
}

fn missing_param_index(
  params: List(option.Option(String)),
  provided: set.Set(String),
  index: Int,
) -> Result(Int, Nil) {
  case params {
    [] -> Error(Nil)
    [param, ..rest] ->
      case param {
        option.None -> Ok(index)
        option.Some(label) ->
          case set.contains(provided, label) {
            True -> missing_param_index(rest, provided, index + 1)
            False -> Ok(index)
          }
      }
  }
}

fn label_index_map(
  params: List(option.Option(String)),
) -> dict.Dict(String, Int) {
  let #(_idx, label_map) =
    list.fold(params, #(0, dict.new()), fn(acc, param) {
      let #(idx, map) = acc
      let map2 = case param {
        option.Some(label) -> dict.insert(map, label, idx)
        option.None -> map
      }
      #(idx + 1, map2)
    })
  label_map
}

fn split_at_param_index(
  arguments: List(g.Field(g.Expression)),
  label_map: dict.Dict(String, Int),
  pipe_index: Int,
  acc: List(g.Field(g.Expression)),
) -> Result(#(List(g.Field(g.Expression)), List(g.Field(g.Expression))), Nil) {
  case arguments {
    [] -> Ok(#(list.reverse(acc), []))
    [first, ..rest] ->
      case arg_label(first) {
        option.Some(label) ->
          case dict.get(label_map, label) {
            Ok(label_index) ->
              case label_index > pipe_index {
                True -> Ok(#(list.reverse(acc), [first, ..rest]))
                False ->
                  split_at_param_index(rest, label_map, pipe_index, [
                    first,
                    ..acc
                  ])
              }
            Error(_) -> Error(Nil)
          }
        option.None -> Error(Nil)
      }
  }
}

fn arg_label(field: g.Field(g.Expression)) -> option.Option(String) {
  case field {
    g.LabelledField(label, _loc, _item) -> option.Some(label)
    g.ShorthandField(label, _loc) -> option.Some(label)
    g.UnlabelledField(_) -> option.None
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
      case literals.parse_int_literal(value) {
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
          infer_bitstring_pat_segment(tenv, span, segment)
        }),
      )
      let scope =
        list.fold(scopes, dict.new(), fn(acc, scope) { dict.merge(acc, scope) })
      is.ok(#(types.Tcon("BitString", span), scope))
    }

    g.PatternVariant(span, module, name, arguments, with_spread) -> {
      case resolve_pattern_constructor(tenv, module, name) {
        Error(_) -> is.error("Unknown type constructor " <> name, span)
        Ok(constructor_name) -> {
          use args2 <- is.bind(instantiate_tcon(tenv, constructor_name, span))
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

fn resolve_pattern_constructor(
  tenv: env.TEnv,
  module: option.Option(String),
  name: String,
) -> Result(String, Nil) {
  case module {
    option.None -> Ok(name)
    option.Some(module_name) ->
      case env.resolve_module(tenv, module_name) {
        Ok(module_key) -> Ok(module_key <> "/" <> name)
        Error(_) -> Error(Nil)
      }
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
        use _ignored <- is.bind(infer_expr_inner(tenv, expr))
        is.ok(Nil)
      }
      _ -> is.ok(Nil)
    }
  })
}

fn infer_bitstring_pat_segment(
  tenv: env.TEnv,
  span: g.Span,
  segment: #(g.Pattern, List(g.BitStringSegmentOption(g.Pattern))),
) -> is.InferState(dict.Dict(String, scheme.Scheme)) {
  let #(pat, options) = segment
  use tuple <- is.bind(infer_pattern(tenv, pat))
  let #(pat_type, scope) = tuple
  let segment_type = bitstring_segment_type(options, span)
  use _ignored <- is.bind(unify(pat_type, segment_type, span))
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

fn bitstring_segment_type(
  options: List(g.BitStringSegmentOption(g.Pattern)),
  span: g.Span,
) -> types.Type {
  case
    {
      has_option(options, fn(opt) {
        case opt {
          g.FloatOption -> True
          _ -> False
        }
      })
    }
  {
    True -> types.Tcon("Float", span)
    False ->
      case
        {
          has_option(options, fn(opt) {
            case opt {
              g.Utf8CodepointOption -> True
              g.Utf16CodepointOption -> True
              g.Utf32CodepointOption -> True
              _ -> False
            }
          })
        }
      {
        True -> types.Tcon("UtfCodepoint", span)
        False ->
          case
            {
              has_option(options, fn(opt) {
                case opt {
                  g.BytesOption -> True
                  g.BitsOption -> True
                  _ -> False
                }
              })
            }
          {
            True -> types.Tcon("BitString", span)
            False -> types.Tcon("Int", span)
          }
      }
  }
}

fn has_option(
  options: List(g.BitStringSegmentOption(g.Pattern)),
  predicate: fn(g.BitStringSegmentOption(g.Pattern)) -> Bool,
) -> Bool {
  list.any(options, predicate)
}

pub fn instantiate_tcon(
  tenv: env.TEnv,
  name: String,
  span: g.Span,
) -> is.InferState(#(List(#(option.Option(String), types.Type)), types.Type)) {
  let env.TEnv(
    _values,
    tcons,
    _types,
    _aliases,
    _modules,
    _params,
    _type_names,
    _refinements,
  ) = tenv
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
        False -> {
          let len1 = list.length(args1)
          let len2 = list.length(args2)
          case len1 > len2 {
            True -> {
              let #(prefix, rest) = split_list_at(args1, len2)
              use _ignored <- is.bind(
                is.each_list(list.zip(prefix, args2), fn(args) {
                  let #(left, right) = args
                  unify(left, right, span)
                }),
              )
              let curried = types.Tfn(rest, res1, span)
              unify(res2, curried, span)
            }
            False -> {
              let #(prefix, rest) = split_list_at(args2, len1)
              use _ignored <- is.bind(
                is.each_list(list.zip(args1, prefix), fn(args) {
                  let #(left, right) = args
                  unify(left, right, span)
                }),
              )
              let curried = types.Tfn(rest, res2, span)
              unify(res1, curried, span)
            }
          }
        }
      }
    types.Ttuple(args1, _), types.Ttuple(args2, _) ->
      case is_open_tuple(args1) || is_open_tuple(args2) {
        True -> {
          use handled <- is.bind(tuple_prefix_unify(args1, args2, span))
          case handled {
            True -> is.ok(Nil)
            False ->
              is.error(
                "Incompatible tuple arity: "
                  <> types.type_to_string(t1)
                  <> " vs "
                  <> types.type_to_string(t2),
                span,
              )
          }
        }
        False ->
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

fn tuple_prefix_unify(
  args1: List(types.Type),
  args2: List(types.Type),
  span: g.Span,
) -> is.InferState(Bool) {
  let open1 = is_open_tuple(args1)
  let open2 = is_open_tuple(args2)
  let has_open = open1 || open2
  let core1 = tuple_core(args1, open1)
  let core2 = tuple_core(args2, open2)
  let len1 = list.length(core1)
  let len2 = list.length(core2)
  case len1 == len2 && has_open {
    True -> {
      let pairs = list.zip(core1, core2)
      use _ignored <- is.bind(
        is.each_list(pairs, fn(args) {
          let #(left, right) = args
          unify(left, right, span)
        }),
      )
      is.ok(True)
    }
    False ->
      case len1 < len2 && open1 {
        True -> {
          let pairs = list.zip(core1, list.take(core2, len1))
          use _ignored <- is.bind(
            is.each_list(pairs, fn(args) {
              let #(left, right) = args
              unify(left, right, span)
            }),
          )
          is.ok(True)
        }
        False ->
          case len2 < len1 && open2 {
            True -> {
              let pairs = list.zip(list.take(core1, len2), core2)
              use _ignored <- is.bind(
                is.each_list(pairs, fn(args) {
                  let #(left, right) = args
                  unify(left, right, span)
                }),
              )
              is.ok(True)
            }
            False -> is.ok(False)
          }
      }
  }
}

fn is_open_tuple(args: List(types.Type)) -> Bool {
  case list.reverse(args) {
    [types.Tcon("tuple_open", _), ..] -> True
    _ -> False
  }
}

fn tuple_core(args: List(types.Type), open: Bool) -> List(types.Type) {
  case open {
    True -> list.take(args, list.length(args) - 1)
    False -> args
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
    True -> is.ok([types.Tcon("tuple_open", span)])
    False -> {
      use rest <- is.bind(tuple_index_vars(count - 1, span))
      use v <- is.bind(new_type_var("tuple_item", span))
      is.ok([v, ..rest])
    }
  }
}

fn infer_record_field_access(
  tenv: env.TEnv,
  span: g.Span,
  container: g.Expression,
  label: String,
) -> is.InferState(types.Type) {
  use target_type <- is.bind(infer_expr_inner(tenv, container))
  use applied_target <- is.bind(type_apply_state(target_type))
  case applied_target {
    types.Tvar(_, _) ->
      case unique_constructor_for_label(tenv, label) {
        Ok(constructor_name) -> {
          use args2 <- is.bind(instantiate_tcon(tenv, constructor_name, span))
          let #(cfields, cres) = args2
          use _ignored <- is.bind(unify(applied_target, cres, span))
          use ctype <- is.bind(lookup_constructor_field(label, cfields, span))
          type_apply_state(ctype)
        }
        Error(_) -> field_type_for_any_constructor_label(tenv, label, span)
      }
    _ -> field_type_for_record_access(tenv, applied_target, label, span)
  }
}

fn field_type_for_any_constructor_label(
  tenv: env.TEnv,
  label: String,
  span: g.Span,
) -> is.InferState(types.Type) {
  let env.TEnv(
    _values,
    tcons,
    _types,
    _aliases,
    _modules,
    _params,
    _type_names,
    _refinements,
  ) = tenv
  use field_type <- is.bind(new_type_var("record_field", span))
  use matched <- is.bind(
    is.foldl_list(dict.to_list(tcons), False, fn(found, pair) {
      let #(name, #(_free, cfields, _cres)) = pair
      case field_has_label(cfields, label) {
        True -> {
          use args2 <- is.bind(instantiate_tcon(tenv, name, span))
          let #(inst_fields, _inst_res) = args2
          use ctype <- is.bind(lookup_constructor_field(
            label,
            inst_fields,
            span,
          ))
          use _ignored <- is.bind(unify(field_type, ctype, span))
          is.ok(True)
        }
        False -> is.ok(found)
      }
    }),
  )
  case matched {
    True -> type_apply_state(field_type)
    False -> is.error("Record field access requires known type", span)
  }
}

fn field_type_for_record_access(
  tenv: env.TEnv,
  applied_target: types.Type,
  label: String,
  span: g.Span,
) -> is.InferState(types.Type) {
  use tcon <- is.bind(
    is.from_result(types.tcon_and_args(applied_target, [], span)),
  )
  let #(tname, targs) = tcon
  let env.TEnv(
    _values,
    tcons,
    types_map,
    _aliases,
    _modules,
    _params,
    _type_names,
    _refinements,
  ) = tenv
  case dict.get(types_map, tname) {
    Error(_) -> is.error("Unknown type name " <> tname, span)
    Ok(#(_arity, names)) -> {
      let constructors = set.to_list(names)
      case constructors {
        [cname] ->
          field_type_for_constructor(tenv, cname, applied_target, label, span)
        _ ->
          field_type_for_constructors(tcons, constructors, targs, label, span)
      }
    }
  }
}

fn field_type_for_constructors(
  tcons: dict.Dict(
    String,
    #(List(String), List(#(option.Option(String), types.Type)), types.Type),
  ),
  constructors: List(String),
  targs: List(types.Type),
  label: String,
  span: g.Span,
) -> is.InferState(types.Type) {
  use field_type <- is.bind(new_type_var("record_field", span))
  use _ignored <- is.bind(
    is.each_list(constructors, fn(constructor_name) {
      case dict.get(tcons, constructor_name) {
        Ok(value) -> {
          let #(free, cfields, _cres) = value
          let subst = dict.from_list(list.zip(free, targs))
          use ctype <- is.bind(lookup_constructor_field(label, cfields, span))
          let ctor_field_type = types.type_apply(subst, ctype)
          unify(field_type, ctor_field_type, span)
        }
        Error(_) -> is.error("Unknown constructor " <> constructor_name, span)
      }
    }),
  )
  type_apply_state(field_type)
}

fn field_type_for_constructor(
  tenv: env.TEnv,
  constructor_name: String,
  applied_target: types.Type,
  label: String,
  span: g.Span,
) -> is.InferState(types.Type) {
  use tcon <- is.bind(
    is.from_result(types.tcon_and_args(applied_target, [], span)),
  )
  let #(tname, targs) = tcon
  let env.TEnv(
    _values,
    tcons,
    types_map,
    _aliases,
    _modules,
    _params,
    _type_names,
    _refinements,
  ) = tenv
  case dict.get(types_map, tname) {
    Error(_) -> is.error("Unknown type name " <> tname, span)
    Ok(#(_arity, names)) ->
      case constructor_name_for_type(names, constructor_name, tname) {
        Error(_) ->
          is.error(
            "Constructor "
              <> constructor_name
              <> " does not belong to type "
              <> tname,
            span,
          )
        Ok(resolved_name) ->
          case dict.get(tcons, resolved_name) {
            Ok(value) -> {
              let #(free, cfields, _cres) = value
              let subst = dict.from_list(list.zip(free, targs))
              use ctype <- is.bind(lookup_constructor_field(
                label,
                cfields,
                span,
              ))
              is.ok(types.type_apply(subst, ctype))
            }
            Error(_) -> is.error("Unknown constructor " <> resolved_name, span)
          }
      }
  }
}

fn constructor_name_for_type(
  names: set.Set(String),
  constructor_name: String,
  type_name: String,
) -> Result(String, Nil) {
  case set.contains(names, constructor_name) {
    True -> Ok(constructor_name)
    False ->
      case type_module_prefix(type_name) {
        option.None -> Error(Nil)
        option.Some(prefix) -> {
          let qualified = prefix <> "/" <> constructor_name
          case set.contains(names, qualified) {
            True -> Ok(qualified)
            False -> Error(Nil)
          }
        }
      }
  }
}

fn unique_constructor_for_label(
  tenv: env.TEnv,
  label: String,
) -> Result(String, Nil) {
  let env.TEnv(
    _values,
    tcons,
    _types,
    _aliases,
    _modules,
    _params,
    _type_names,
    _refinements,
  ) = tenv
  let matches =
    list.fold(dict.to_list(tcons), [], fn(acc, pair) {
      let #(name, #(_free, cfields, _cres)) = pair
      case field_has_label(cfields, label) {
        True -> [name, ..acc]
        False -> acc
      }
    })
  case list.reverse(matches) {
    [name] -> Ok(name)
    _ -> Error(Nil)
  }
}

fn field_has_label(
  cfields: List(#(option.Option(String), types.Type)),
  label: String,
) -> Bool {
  list.any(cfields, fn(field) {
    let #(maybe, _t) = field
    case maybe {
      option.Some(name) -> name == label
      option.None -> False
    }
  })
}

fn type_module_prefix(type_name: String) -> option.Option(String) {
  let parts = string.split(type_name, "/")
  case list.reverse(parts) {
    [_last] -> option.None
    [_, ..rest_rev] -> {
      let rest = list.reverse(rest_rev)
      option.Some(string.join(rest, "/"))
    }
    [] -> option.None
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
